/*
 * Copyright 2018-2025 Scala Steward contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scalasteward.core.forge.github

import cats.MonadThrow
import cats.syntax.all.*
import io.circe.syntax.*
import io.circe.{Decoder, Json}
import java.nio.charset.StandardCharsets.US_ASCII
import org.http4s.{Request, Uri}
import org.scalasteward.core.data.Repo
import org.scalasteward.core.forge.github.GitHubCommitPublisher.*
import org.scalasteward.core.git.CommitSeries.TreeChange
import org.scalasteward.core.git.{Branch, CommitSeries, CommitSeriesAlg, Sha1}
import org.scalasteward.core.nurture.UpdateBranchPublisher
import org.scalasteward.core.util.HttpJsonClient
import org.typelevel.log4cats.Logger
import scodec.bits.ByteVector

/** Recreates the local commit series through the Git database API instead of pushing it.
  *
  * Authenticated as a GitHub App and sending no author, committer or signature, each commit is
  * created and signed by GitHub as the App, so it is verified without a signing key or a user
  * account. Every recreated commit gets a new SHA; references to an earlier commit of the series in
  * later file contents or messages (such as `.git-blame-ignore-revs`) are rewritten to the new SHA.
  *
  * @see
  *   [[https://docs.github.com/en/rest/git]]
  */
final class GitHubCommitPublisher[F[_]](
    gitHubApiHost: Uri,
    modify: Request[F] => F[Request[F]],
    commitSeriesAlg: CommitSeriesAlg[F]
)(implicit
    client: HttpJsonClient[F],
    logger: Logger[F],
    F: MonadThrow[F]
) extends UpdateBranchPublisher[F] {
  private val repos = new Url(gitHubApiHost).repos(_)

  override def publish(repo: Repo, base: Branch, branch: Branch): F[Unit] =
    for {
      series <- commitSeriesAlg.commitSeries(repo, base, branch)
      _ <- F.raiseWhen(series.commits.isEmpty)(
        new IllegalStateException(s"${branch.name} has no commits on top of ${base.name}")
      )
      initial = Recreated(series.base, series.baseTree, Map.empty, rewritten = false)
      last <- series.commits.foldLeftM(initial)(recreate(repo, _, _))
      _ <- updateRef(repo, branch, last.commit)
      _ <- logger.info(s"Published ${series.commits.size} verified commit(s) to ${branch.name}")
    } yield ()

  private def recreate(repo: Repo, prev: Recreated, entry: CommitSeries.Entry): F[Recreated] =
    for {
      entries <- entry.changes.traverse(treeEntry(repo, prev.replacements, _))
      rewritten = prev.rewritten || entries.exists(_._2)
      tree <- post[Sha](
        repos(repo) / "git" / "trees",
        Json.obj("base_tree" := prev.tree, "tree" := entries.map(_._1))
      )
      _ <- F.raiseUnless(rewritten || tree.sha === entry.tree)(
        new IllegalStateException(
          s"GitHub built tree ${tree.sha.value} for commit ${entry.sha1.value}, expected ${entry.tree.value}"
        )
      )
      message = prev.replacements.foldLeft(entry.message) { case (msg, (from, to)) =>
        msg.replace(from.value.value, to.value.value)
      }
      commit <- post[Sha](
        repos(repo) / "git" / "commits",
        Json.obj("message" := message, "tree" := tree.sha, "parents" := List(prev.commit))
      )
      _ <- logger.info(s"Recreated ${entry.sha1.value} as verified commit ${commit.sha.value}")
    } yield Recreated(
      commit.sha,
      tree.sha,
      prev.replacements.updated(entry.sha1, commit.sha),
      rewritten
    )

  /** The tree entry for `change`, and whether its content differs from the local blob. */
  private def treeEntry(
      repo: Repo,
      replacements: Map[Sha1, Sha1],
      change: TreeChange
  ): F[(Json, Boolean)] =
    change match {
      case TreeChange.Delete(path, mode) =>
        F.pure((entryJson(path, mode, Json.Null), false))
      case TreeChange.Write(path, mode, sha1) if mode === TreeChange.submoduleMode =>
        F.pure((entryJson(path, mode, sha1.asJson), false))
      case TreeChange.Write(path, mode, sha1) =>
        for {
          local <- commitSeriesAlg.blobContent(repo, sha1)
          content = rewriteReferences(local, replacements)
          blob <- post[Sha](
            repos(repo) / "git" / "blobs",
            Json.obj("content" := content.toBase64, "encoding" := "base64")
          )
          expected = blobSha1(content)
          _ <- F.raiseUnless(blob.sha.value.value === expected)(
            new IllegalStateException(
              s"GitHub stored $path as blob ${blob.sha.value}, expected $expected"
            )
          )
        } yield (entryJson(path, mode, blob.sha.asJson), !(content === local))
    }

  private def updateRef(repo: Repo, branch: Branch, commit: Sha1): F[Unit] = {
    val ref = s"refs/heads/${branch.name}"
    client
      .get[List[RefOut]](
        branchPath(repos(repo) / "git" / "matching-refs" / "heads", branch),
        modify
      )
      .map(_.exists(_.ref === ref))
      .flatMap {
        case true =>
          client.patchWithBody[Json, Json](
            branchPath(repos(repo) / "git" / "refs" / "heads", branch),
            Json.obj("sha" := commit, "force" := true),
            modify
          )
        case false =>
          post[Json](repos(repo) / "git" / "refs", Json.obj("ref" := ref, "sha" := commit))
      }
      .void
  }

  private def post[A: Decoder](uri: Uri, body: Json): F[A] =
    client.postWithBody[A, Json](uri, body, modify)
}

object GitHubCommitPublisher {
  final private case class Recreated(
      commit: Sha1,
      tree: Sha1,
      replacements: Map[Sha1, Sha1],
      rewritten: Boolean
  )

  final private case class Sha(sha: Sha1)
  private object Sha {
    implicit val shaDecoder: Decoder[Sha] = Decoder.forProduct1("sha")(Sha.apply)
  }

  final private case class RefOut(ref: String)
  private object RefOut {
    implicit val refOutDecoder: Decoder[RefOut] = Decoder.forProduct1("ref")(RefOut.apply)
  }

  // Each component of a branch name such as `update/cats-core-2.13.0` is its own path segment.
  private def branchPath(prefix: Uri, branch: Branch): Uri =
    branch.name.split('/').foldLeft(prefix)(_ / _)

  private def entryJson(path: String, mode: String, sha: Json): Json = {
    val tpe = if (mode === TreeChange.submoduleMode) "commit" else "blob"
    Json.obj("path" := path, "mode" := mode, "type" := tpe, "sha" := sha)
  }

  /** The object ID git assigns to a blob holding `content`. */
  private[github] def blobSha1(content: ByteVector): String =
    (ByteVector.view(s"blob ${content.size}\u0000".getBytes(US_ASCII)) ++ content)
      .digest("SHA-1")
      .toHex

  private[github] def rewriteReferences(
      content: ByteVector,
      replacements: Map[Sha1, Sha1]
  ): ByteVector =
    replacements.foldLeft(content) { case (acc, (from, to)) =>
      replaceAll(acc, ascii(from), ascii(to))
    }

  private def ascii(sha1: Sha1): ByteVector =
    ByteVector.view(sha1.value.value.getBytes(US_ASCII))

  private def replaceAll(content: ByteVector, from: ByteVector, to: ByteVector): ByteVector = {
    val i = content.indexOfSlice(from)
    if (i < 0) content
    else content.take(i) ++ to ++ replaceAll(content.drop(i + from.size), from, to)
  }
}
