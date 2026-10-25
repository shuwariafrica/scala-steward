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

import better.files.File
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import java.nio.file.attribute.PosixFilePermission
import munit.CatsEffectSuite
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.http4s.syntax.literals.*
import org.http4s.{HttpApp, Request}
import org.scalasteward.core.TestInstances.ioLogger
import org.scalasteward.core.data.Repo
import org.scalasteward.core.git.FileGitAlgTest.{ioAuxGitAlg, ioGitAlg}
import org.scalasteward.core.git.{Branch, CommitMsg, CommitSeriesAlg}
import org.scalasteward.core.io.FileAlgTest.ioFileAlg
import org.scalasteward.core.io.ProcessAlgTest.ioProcessAlg
import org.scalasteward.core.mock.MockConfig.mockRoot
import org.scalasteward.core.util.{HttpJsonClient, Nel}
import scodec.bits.ByteVector

/** Publishes a local series to a fake Git database API backed by a second repository, so that the
  * trees and commit SHAs the publisher sees are the ones git computes.
  */
class GitHubCommitPublisherTest extends CatsEffectSuite {
  private val rootDir = mockRoot / "github-commit-publisher"
  private val local = rootDir / "local"
  private val remote = rootDir / "remote"
  private val repo = Repo("owner", "repo")
  private val master = Branch("master")
  private val update = Branch("update/cats-core-2.13.0")
  private val committerEnv = List(
    "GIT_AUTHOR_NAME" -> "app[bot]",
    "GIT_AUTHOR_EMAIL" -> "app@example.org",
    "GIT_COMMITTER_NAME" -> "app[bot]",
    "GIT_COMMITTER_EMAIL" -> "app@example.org"
  )

  private def git(dir: File, env: List[(String, String)] = Nil)(args: String*): IO[String] =
    ioProcessAlg.exec(Nel.of("git", args*), dir, env).map(_.mkString("\n").trim)

  private def json(req: Request[IO]): IO[Json] = req.as[Json]

  private def str(j: Json, field: String): String =
    j.hcursor.downField(field).as[String].fold(throw _, identity)

  private val fakeGitHub: HttpApp[IO] = {
    val base = Root / "repos" / "owner" / "repo" / "git"
    HttpApp[IO] {
      case req @ POST -> `base` / "blobs" =>
        json(req).flatMap { body =>
          val file = rootDir / "blob"
          val bytes = ByteVector.fromValidBase64(str(body, "content"))
          IO.blocking(file.writeByteArray(bytes.toArray)) >>
            git(remote)("hash-object", "-w", "--no-filters", file.pathAsString)
              .flatMap(sha => Created(Json.obj("sha" -> Json.fromString(sha))))
        }
      case req @ POST -> `base` / "trees" =>
        json(req).flatMap { body =>
          val env = List("GIT_INDEX_FILE" -> (rootDir / "index").pathAsString)
          val entries = body.hcursor.downField("tree").as[List[Json]].fold(throw _, identity)
          git(remote, env)("read-tree", str(body, "base_tree")) >>
            entries.traverse_ { e =>
              val path = str(e, "path")
              e.hcursor.downField("sha").as[Option[String]].fold(throw _, identity) match {
                case None      => git(remote, env)("update-index", "--force-remove", path)
                case Some(sha) =>
                  git(remote, env)(
                    "update-index",
                    "--add",
                    "--cacheinfo",
                    s"${str(e, "mode")},$sha,$path"
                  )
              }
            } >>
            git(remote, env)("write-tree").flatMap(sha =>
              Created(Json.obj("sha" -> Json.fromString(sha)))
            )
        }
      case req @ POST -> `base` / "commits" =>
        json(req).flatMap { body =>
          val parents = body.hcursor.downField("parents").as[List[String]].fold(throw _, identity)
          val args = List("commit-tree", str(body, "tree")) ++
            parents.flatMap(p => List("-p", p)) ++ List("-m", str(body, "message"))
          git(remote, committerEnv)(args*).flatMap(sha =>
            Created(Json.obj("sha" -> Json.fromString(sha)))
          )
        }
      case GET -> `base` / "matching-refs" / "heads" / "update" / name =>
        git(remote)("for-each-ref", "--format=%(refname)", s"refs/heads/update/$name").flatMap {
          out =>
            Ok(
              Json.arr(
                out.linesIterator
                  .filter(_.nonEmpty)
                  .map(r => Json.obj("ref" -> Json.fromString(r)))
                  .toSeq*
              )
            )
        }
      case req @ POST -> `base` / "refs" =>
        json(req).flatMap { body =>
          git(remote)("update-ref", str(body, "ref"), str(body, "sha")) >> Created(body)
        }
      case req @ PATCH -> `base` / "refs" / "heads" / "update" / name =>
        json(req).flatMap { body =>
          git(remote)("update-ref", s"refs/heads/update/$name", str(body, "sha")) >> Ok(body)
        }
      case _ => NotFound()
    }
  }

  implicit private val client: HttpJsonClient[IO] = {
    implicit val c: Client[IO] = Client.fromHttpApp(fakeGitHub)
    new HttpJsonClient[IO]
  }

  private val commitSeriesAlg = new CommitSeriesAlg[IO](_ => IO.pure(local))

  private val publisher =
    new GitHubCommitPublisher[IO](uri"https://api.github.com", IO.pure(_), commitSeriesAlg)

  private val binary = ByteVector(0x00, 0xff, 0x0d, 0x0a, 0x80)

  private def setUp: IO[(String, String)] =
    for {
      _ <- ioFileAlg.deleteForce(rootDir)
      _ <- ioAuxGitAlg.createRepo(local)
      _ <- ioFileAlg.writeFile(local / "build.sbt", "libraryDependencies += cats % \"2.12.0\"\n")
      _ <- IO.blocking((local / "data.bin").writeByteArray(binary.toArray))
      _ <- ioFileAlg.writeFile(local / "gone.txt", "removed by the update")
      _ <- git(local)("add", "--all")
      _ <- ioGitAlg.commitAll(local, CommitMsg("Add files"), signoffCommits = None)
      _ <- git(rootDir)(
        "clone",
        "--quiet",
        "--no-checkout",
        local.pathAsString,
        remote.pathAsString
      )
      _ <- ioGitAlg.createBranch(local, update)
      _ <- ioFileAlg.writeFile(local / "build.sbt", "libraryDependencies += cats % \"2.13.0\"")
      _ <- ioFileAlg.deleteForce(local / "gone.txt")
      _ <- IO.blocking((local / "data.bin").writeByteArray((binary ++ binary).toArray))
      _ <- ioFileAlg.writeFile(local / "run.sh", "#!/bin/sh\n")
      _ <- IO.blocking((local / "run.sh").addPermission(PosixFilePermission.OWNER_EXECUTE))
      _ <- git(local)("add", "--all")
      first <- ioGitAlg.commitAll(local, CommitMsg("Update cats-core to 2.13.0"), None)
      firstSha = first.sha1.value.value
      _ <- ioFileAlg.writeFile(local / ".git-blame-ignore-revs", s"# reformat\n$firstSha\n")
      _ <- git(local)("add", ".git-blame-ignore-revs")
      _ <- ioGitAlg.commitAll(local, CommitMsg(s"Ignore $firstSha in blame"), None)
      firstTree <- git(local)("rev-parse", s"$firstSha^{tree}")
    } yield (firstSha, firstTree)

  test("publish recreates the series and rewrites references to recreated commits") {
    for {
      first <- setUp
      (localFirst, localFirstTree) = first
      _ <- publisher.publish(repo, master, update)
      remoteFirst <- git(remote)("rev-parse", s"${update.name}~1")
      remoteFirstTree <- git(remote)("rev-parse", s"${update.name}~1^{tree}")
      parent <- git(remote)("rev-parse", s"${update.name}~2")
      remoteMaster <- git(remote)("rev-parse", master.name)
      blameIgnore <- git(remote)("show", s"${update.name}:.git-blame-ignore-revs")
      message <- git(remote)("log", "-1", "--format=%B", update.name)
      mode <- git(remote)("ls-tree", update.name, "run.sh")
      bin <- git(remote)("cat-file", "-s", s"${update.name}:data.bin")
      gone <- git(remote)("ls-tree", "--name-only", update.name, "gone.txt")
    } yield {
      assertEquals(remoteFirstTree, localFirstTree)
      assertEquals(parent, remoteMaster)
      assertNotEquals(remoteFirst, localFirst)
      assertEquals(blameIgnore, s"# reformat\n$remoteFirst")
      assertEquals(message, s"Ignore $remoteFirst in blame")
      assert(clue(mode).startsWith("100755"))
      assertEquals(bin, (binary.size * 2).toString)
      assertEquals(gone, "")
    }
  }

  test("publish force-moves an existing remote branch") {
    for {
      _ <- setUp
      _ <- publisher.publish(repo, master, update)
      before <- git(remote)("rev-parse", update.name)
      _ <- ioGitAlg.resetHard(local, master)
      _ <- ioFileAlg.writeFile(local / "build.sbt", "libraryDependencies += cats % \"2.13.1\"")
      _ <- ioGitAlg.commitAll(local, CommitMsg("Update cats-core to 2.13.1"), None)
      _ <- publisher.publish(repo, master, update)
      after <- git(remote)("rev-parse", update.name)
      parent <- git(remote)("rev-parse", s"${update.name}~1")
      remoteMaster <- git(remote)("rev-parse", master.name)
    } yield {
      assertNotEquals(after, before)
      assertEquals(parent, remoteMaster)
    }
  }

  test("publish refuses a branch with a merge commit") {
    val result = for {
      _ <- setUp
      _ <- git(local)("checkout", "--quiet", "-b", "side", master.name)
      _ <- ioFileAlg.writeFile(local / "side.txt", "side")
      _ <- git(local)("add", "side.txt")
      _ <- ioGitAlg.commitAll(local, CommitMsg("Side"), None)
      _ <- git(local)("checkout", "--quiet", update.name)
      _ <- git(local)("merge", "--no-edit", "--no-ff", "side")
      _ <- publisher.publish(repo, master, update)
    } yield ()
    result.attempt.map(r => assert(r.isLeft, clue(r)))
  }
}
