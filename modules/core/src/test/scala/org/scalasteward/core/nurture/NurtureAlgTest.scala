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

package org.scalasteward.core.nurture

import better.files.File
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import munit.CatsEffectSuite
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpApp, Request, Response}
import org.scalasteward.core.TestInstances.*
import org.scalasteward.core.TestSyntax.*
import org.scalasteward.core.data.{
  DependencyInfo,
  ProcessResult,
  Repo,
  RepoData,
  Update,
  UpdateData
}
import org.scalasteward.core.edit.EditAttempt.UpdateEdit
import org.scalasteward.core.forge.data.{NewPullRequestData, PullRequestNumber}
import org.scalasteward.core.git.{Branch, Commit}
import org.scalasteward.core.mock.MockContext.context
import org.scalasteward.core.mock.MockState.TraceEntry.{Cmd, Log}
import org.scalasteward.core.mock.{GitHubAuth, MockConfig, MockEff, MockEffOps, MockState}
import org.scalasteward.core.repoconfig.{PullRequestsConfig, RepoConfig}

class NurtureAlgTest extends CatsEffectSuite with Http4sDsl[MockEff] {
  test("preparePullRequest") {
    val nurtureAlg = context.nurtureAlg
    val repo = Repo("scala-steward-org", "scala-steward")
    val dependency = "org.typelevel".g % ("cats-effect", "cats-effect_2.13").a % "3.3.0"
    val repoCache = dummyRepoCache.copy(dependencyInfos =
      List(List(DependencyInfo(dependency, Nil)).withMavenCentral)
    )
    val repoData = RepoData(
      repo,
      repoCache,
      RepoConfig(assignees = List("foo").some, reviewers = List("bar").some)
    )
    val fork = Repo("scala-steward", "scala-steward")
    val update = (dependency %> "3.4.0").single
    val baseBranch = Branch("main")
    val updateBranch = Branch("update/cats-effect-3.4.0")
    val updateData = UpdateData(repoData, fork, update, baseBranch, dummySha1, updateBranch)
    val edits = List(UpdateEdit(update, Commit(dummySha1)))
    val state = MockState.empty.copy(clientResponses = GitHubAuth.api(List.empty) <+> HttpApp {
      case HEAD -> Root / "typelevel" / "cats-effect"                                 => Ok()
      case HEAD -> Root / "typelevel" / "cats-effect" / "releases" / "tag" / "v3.4.0" => Ok()
      case HEAD -> Root / "typelevel" / "cats-effect" / "compare" / "v3.3.0...v3.4.0" => Ok()
      case _                                                                          => NotFound()
    })
    val obtained = nurtureAlg.preparePullRequest(updateData, edits).runA(state)
    val expected = NewPullRequestData(
      title = "Update cats-effect to 3.4.0",
      body =
        raw"""## About this PR
             |📦 Updates [org.typelevel:cats-effect](https://github.com/typelevel/cats-effect) from `3.3.0` to `3.4.0`
             |
             |📜 [GitHub Release Notes](https://github.com/typelevel/cats-effect/releases/tag/v3.4.0) - [Version Diff](https://github.com/typelevel/cats-effect/compare/v3.3.0...v3.4.0)
             |
             |## Usage
             |✅ **Please merge!**
             |
             |I'll automatically update this PR to resolve conflicts as long as you don't change it yourself.
             |
             |If you'd like to skip this version, you can just close this PR. If you have any feedback, just mention me in the comments below.
             |
             |Configure Scala Steward for your repository with a [`.scala-steward.conf`](https://github.com/scala-steward-org/scala-steward/blob/${org.scalasteward.core.BuildInfo.gitHeadCommit}/docs/repo-specific-configuration.md) file.
             |
             |_Have a fantastic day writing Scala!_
             |
             |<details>
             |<summary>⚙ Adjust future updates</summary>
             |
             |Add this to your `.scala-steward.conf` file to ignore future updates of this dependency:
             |```
             |updates.ignore = [ { groupId = "org.typelevel", artifactId = "cats-effect" } ]
             |```
             |Or, add this to slow down future updates of this dependency:
             |```
             |dependencyOverrides = [{
             |  pullRequests = { frequency = "30 days" },
             |  dependency = { groupId = "org.typelevel", artifactId = "cats-effect" }
             |}]
             |```
             |</details>
             |
             |<sup>
             |labels: library-update, early-semver-minor, semver-spec-minor, version-scheme:early-semver, commit-count:1
             |</sup>
             |
             |<!-- scala-steward = {
             |  "Update" : {
             |    "ForArtifactId" : {
             |      "crossDependency" : [
             |        {
             |          "groupId" : "org.typelevel",
             |          "artifactId" : {
             |            "name" : "cats-effect",
             |            "maybeCrossName" : "cats-effect_2.13"
             |          },
             |          "version" : "3.3.0",
             |          "sbtVersion" : null,
             |          "scalaVersion" : null,
             |          "configurations" : null
             |        }
             |      ],
             |      "newerVersions" : [
             |        "3.4.0"
             |      ],
             |      "newerGroupId" : null,
             |      "newerArtifactId" : null
             |    }
             |  },
             |  "Labels" : [
             |    "library-update",
             |    "early-semver-minor",
             |    "semver-spec-minor",
             |    "version-scheme:early-semver",
             |    "commit-count:1"
             |  ]
             |} -->""".stripMargin.trim,
      head = "scala-steward:update/cats-effect-3.4.0",
      base = baseBranch,
      labels = List(
        "library-update",
        "early-semver-minor",
        "semver-spec-minor",
        "version-scheme:early-semver",
        "commit-count:1"
      ),
      assignees = List("foo"),
      reviewers = List("bar")
    )
    assertIO(obtained, expected)
  }

  test("preparePullRequest should not set labels if ForgeConfig.addLabels = false") {
    def findLabelLine(L: Array[String]) =
      L.zipWithIndex.find(_._1.contains("labels: ")).map(x => L(x._2)).getOrElse("")

    val config =
      MockConfig.config.copy(forgeCfg = MockConfig.config.forgeCfg.copy(addLabels = false))
    val nurtureAlg = context(config).nurtureAlg
    val repo = Repo("scala-steward-org", "scala-steward")
    val dependency = "org.typelevel".g % ("cats-effect", "cats-effect_2.13").a % "3.3.0"
    val repoCache = dummyRepoCache.copy(dependencyInfos =
      List(List(DependencyInfo(dependency, Nil)).withMavenCentral)
    )
    val repoData = RepoData(
      repo,
      repoCache,
      RepoConfig(assignees = List("foo").some, reviewers = List("bar").some)
    )
    val fork = Repo("scala-steward", "scala-steward")
    val update = (dependency %> "3.4.0").single
    val baseBranch = Branch("main")
    val updateBranch = Branch("update/cats-effect-3.4.0")
    val updateData = UpdateData(repoData, fork, update, baseBranch, dummySha1, updateBranch)
    val edits = List(UpdateEdit(update, Commit(dummySha1)))
    val state = MockState.empty.copy(clientResponses = HttpApp {
      case HEAD -> Root / "typelevel" / "cats-effect"                                 => Ok()
      case HEAD -> Root / "typelevel" / "cats-effect" / "releases" / "tag" / "v3.4.0" => Ok()
      case HEAD -> Root / "typelevel" / "cats-effect" / "compare" / "v3.3.0...v3.4.0" => Ok()
      case _                                                                          => NotFound()
    })
    nurtureAlg.preparePullRequest(updateData, edits).runA(state).map { obtained =>
      assert(obtained.labels.isEmpty)
      val labelLine = findLabelLine(obtained.body.split("\n"))
      assertEquals(
        labelLine,
        "labels: library-update, early-semver-minor, semver-spec-minor, version-scheme:early-semver, commit-count:1"
      )
    }
  }

  test(
    "preparePullRequest should set custom labels if PullRequestsConfig.customLabels is provided"
  ) {
    def findLabelLine(L: Array[String]) =
      L.zipWithIndex.find(_._1.contains("labels: ")).map(x => L(x._2)).getOrElse("")

    val nurtureAlg = context.nurtureAlg
    val dependency = "org.typelevel".g % ("cats-effect", "cats-effect_2.13").a % "3.3.0"
    val customLabels = List("custom-label-1", "custom-label-2")
    val repoData =
      RepoData(
        repo = Repo("scala-steward-org", "scala-steward"),
        cache = dummyRepoCache,
        config =
          RepoConfig(pullRequests = PullRequestsConfig(customLabels = customLabels.some).some)
      )
    val update = (dependency %> "3.4.0").single
    val baseBranch = Branch("main")
    val updateBranch = Branch("update/cats-effect-3.4.0")
    val updateData =
      UpdateData(repoData, repoData.repo, update, baseBranch, dummySha1, updateBranch)
    val edits = List(UpdateEdit(update, Commit(dummySha1)))
    val state = MockState.empty.copy(clientResponses = HttpApp {
      case HEAD -> Root / "typelevel" / "cats-effect"                                 => Ok()
      case HEAD -> Root / "typelevel" / "cats-effect" / "releases" / "tag" / "v3.4.0" => Ok()
      case HEAD -> Root / "typelevel" / "cats-effect" / "compare" / "v3.3.0...v3.4.0" => Ok()
      case _                                                                          => NotFound()
    })
    nurtureAlg.preparePullRequest(updateData, edits).runA(state).map { obtained =>
      assertEquals(
        obtained.labels,
        customLabels ++ List(
          "library-update",
          "early-semver-minor",
          "semver-spec-minor",
          "commit-count:1"
        )
      )

      val labelLine = findLabelLine(obtained.body.split("\n"))

      assertEquals(
        labelLine,
        "labels: custom-label-1, custom-label-2, library-update, early-semver-minor, semver-spec-minor, commit-count:1"
      )
    }
  }

  test("applyNewUpdate: grouped update of artifacts sharing a version val") {
    val repo = Repo("nurture-alg", "grouped-new")
    val repoDir = context.workspaceAlg.repoDir(repo).unsafeRunSync()
    val data = circeUpdateData(repo)
    val initial = MockState.empty.copy(
      execCommands = true,
      commandOutputs = Map(gitPush(repoDir, data.updateBranch) -> Right(List.empty)),
      clientResponses = circeForgeApi(number = 1347)
    )
    for {
      state0 <- initial.initGitRepo(repoDir, repoDir / "build.sbt" -> circeBuildSbt)
      stateAndResult <- context.nurtureAlg.applyNewUpdate(data).runSA(state0)
    } yield {
      val (state, result) = stateAndResult
      assertEquals(result, ProcessResult.Created(PullRequestNumber(1347)))
      val commit = Cmd.gitCommit(repoDir, "Update circe-core, circe-parser to 0.14.4")
      assertEquals(state.trace.count(_ == commit), 1)
      assertEquals(unableToBumpWarnings(state), Vector.empty)
    }
  }

  test("resetAndApplyAgain: grouped update of artifacts sharing a version val") {
    val repo = Repo("nurture-alg", "grouped-existing")
    val repoDir = context.workspaceAlg.repoDir(repo).unsafeRunSync()
    val data = circeUpdateData(repo)
    val initial = MockState.empty.copy(
      execCommands = true,
      commandOutputs = Map(gitPush(repoDir, data.updateBranch) -> Right(List.empty)),
      clientResponses = circeForgeApi(number = 42)
    )
    for {
      state0 <- initial.initGitRepo(repoDir, repoDir / "build.sbt" -> circeBuildSbt)
      state1 <- context.gitAlg.createBranch(repo, data.updateBranch).runS(state0)
      stateAndResult <- context.nurtureAlg
        .resetAndApplyAgain(PullRequestNumber(42), data)
        .runSA(state1)
    } yield {
      val (state, result) = stateAndResult
      assertEquals(result, ProcessResult.Updated)
      assertEquals(
        state.files.get(repoDir / "build.sbt"),
        Some(circeBuildSbt.replace("0.13.0", "0.14.4"))
      )
      assertEquals(unableToBumpWarnings(state), Vector.empty)
    }
  }

  private val master = Branch("master")

  private val circeBuildSbt =
    """val circeVersion = "0.13.0"
      |libraryDependencies ++= Seq(
      |  "io.circe" %% "circe-core" % circeVersion,
      |  "io.circe" %% "circe-parser" % circeVersion
      |)""".stripMargin

  private def circeUpdateData(repo: Repo): UpdateData = {
    val grouped = Update.Grouped(
      "circe",
      Some("Circe updates"),
      List(
        ("io.circe".g % "circe-core".a % "0.13.0" %> "0.14.4").single,
        ("io.circe".g % "circe-parser".a % "0.13.0" %> "0.14.4").single
      )
    )
    val repoData = RepoData(repo, dummyRepoCache, RepoConfig.empty)
    val fork = Repo("scala-steward", repo.repo)
    UpdateData(repoData, fork, grouped, master, dummySha1, Branch("update/circe"))
  }

  private def gitPush(repoDir: File, branch: Branch): Cmd =
    Cmd.git(repoDir, "push", "--force", "--set-upstream", "origin", branch.name)

  private def circeForgeApi(number: Int): HttpApp[MockEff] =
    GitHubAuth.api(List.empty) <+> HttpApp[MockEff] {
      case req @ POST -> Root / "repos" / _ / _ / "pulls" =>
        respondIfBodyListsBothArtifacts(req, number)
      case req @ PATCH -> Root / "repos" / _ / _ / "pulls" / IntVar(_) =>
        respondIfBodyListsBothArtifacts(req, number)
      case POST -> Root / "repos" / _ / _ / "issues" / IntVar(_) / "labels" =>
        Ok(Json.arr())
      case _ => NotFound()
    }

  private def respondIfBodyListsBothArtifacts(
      req: Request[MockEff],
      number: Int
  ): MockEff[Response[MockEff]] =
    req.as[Json].flatMap { json =>
      val body = json.hcursor.get[String]("body").getOrElse("")
      val htmlUrl = s"https://github.com/octocat/Hello-World/pull/$number"
      if (body.contains("circe-core") && body.contains("circe-parser"))
        Ok(json"""{
          "html_url": $htmlUrl,
          "state": "open",
          "number": $number,
          "title": "Circe updates"
        }""")
      else BadRequest(s"PR body does not list both artifacts: $body")
    }

  private def unableToBumpWarnings(state: MockState): Vector[String] =
    state.trace.collect { case Log((_, msg)) if msg.startsWith("Unable to bump version") => msg }
}
