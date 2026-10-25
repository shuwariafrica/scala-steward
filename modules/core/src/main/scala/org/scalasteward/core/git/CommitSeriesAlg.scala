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

package org.scalasteward.core.git

import better.files.File
import cats.effect.Sync
import cats.syntax.all.*
import org.scalasteward.core.data.Repo
import org.scalasteward.core.io.ProcessAlg
import scodec.bits.ByteVector

/** Reads a local branch as a series of commits, with each blob's exact bytes, for recreating it
  * elsewhere.
  */
final class CommitSeriesAlg[F[_]](repoDir: Repo => F[File])(implicit
    processAlg: ProcessAlg[F],
    F: Sync[F]
) {

  /** The commits `branch` adds on top of `base`. Fails unless they form a linear chain. */
  def commitSeries(repo: Repo, base: Branch, branch: Branch): F[CommitSeries] =
    for {
      dir <- repoDir(repo)
      baseSha1 <- git(dir, "merge-base", base.name, branch.name).flatMap(parseSha1)
      baseTree <- treeOf(dir, baseSha1)
      revisions <- git(dir, "rev-list", "--reverse", "--parents", s"${base.name}..${branch.name}")
      entries <- revisions.filter(_.nonEmpty).traverse(entry(dir, _))
      _ <- F.raiseUnless(isChain(baseSha1, entries))(
        new IllegalStateException(s"${branch.name} is not a linear series on top of ${base.name}")
      )
    } yield CommitSeries(baseSha1, baseTree, entries.map(_._2))

  /** The bytes of `blob` as stored, with no filters or line-ending conversion applied. */
  def blobContent(repo: Repo, blob: Sha1): F[ByteVector] =
    for {
      dir <- repoDir(repo)
      name <- git(dir, "unpack-file", blob.value.value)
      file = dir / name.mkString.trim
      bytes <- F.guarantee(
        F.blocking(ByteVector.view(file.byteArray)),
        F.blocking(file.delete(swallowIOExceptions = true)).void
      )
    } yield bytes

  private def entry(dir: File, revision: String): F[(Sha1, CommitSeries.Entry)] =
    revision.split(' ').toList match {
      case List(commit, parent) =>
        for {
          sha1 <- parseSha1(List(commit))
          parentSha1 <- parseSha1(List(parent))
          tree <- treeOf(dir, sha1)
          raw <- git(dir, "cat-file", "commit", commit)
          message = raw.dropWhile(_.nonEmpty).drop(1).mkString("\n")
          diff <- git(
            dir,
            "diff-tree",
            "-r",
            "-z",
            "--no-renames",
            "--no-commit-id",
            parent,
            commit
          )
          changes <- F.fromEither(CommitSeries.TreeChange.parseRawZ(diff.mkString("\n")))
        } yield (parentSha1, CommitSeries.Entry(sha1, tree, message, changes))
      case _ =>
        F.raiseError(new IllegalStateException(s"Not a single-parent commit: $revision"))
    }

  private def isChain(base: Sha1, entries: List[(Sha1, CommitSeries.Entry)]): Boolean =
    entries
      .foldLeft(Option(base)) { case (expected, (parent, entry)) =>
        expected.filter(_ === parent).as(entry.sha1)
      }
      .isDefined

  private def treeOf(dir: File, commit: Sha1): F[Sha1] =
    git(dir, "rev-parse", "--verify", s"${commit.value.value}^{tree}").flatMap(parseSha1)

  private def parseSha1(lines: List[String]): F[Sha1] =
    F.fromEither(Sha1.from(lines.mkString.trim))

  private def git(dir: File, args: String*): F[List[String]] =
    processAlg.exec(FileGitAlg.gitCmd ++ args.toList, dir)
}
