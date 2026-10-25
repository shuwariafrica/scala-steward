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

/** The linear run of commits a branch adds on top of `base`, oldest first. */
final case class CommitSeries(base: Sha1, baseTree: Sha1, commits: List[CommitSeries.Entry])

object CommitSeries {

  /** One commit of the series. `message` is the raw message as git stores it. */
  final case class Entry(sha1: Sha1, tree: Sha1, message: String, changes: List[TreeChange])

  sealed trait TreeChange extends Product with Serializable {
    def path: String
  }

  object TreeChange {

    /** `mode` is git's octal file mode; `160000` marks a submodule, whose `sha1` is a commit. */
    final case class Write(path: String, mode: String, sha1: Sha1) extends TreeChange

    final case class Delete(path: String, mode: String) extends TreeChange

    val submoduleMode: String = "160000"

    private val deletedMode = "000000"

    /** Parses the output of `git diff-tree -r -z --no-renames --no-commit-id`. */
    def parseRawZ(output: String): Either[Throwable, List[TreeChange]] = {
      val fields = output.split('\u0000').toList.filter(_.nonEmpty)
      fields
        .grouped(2)
        .toList
        .foldLeft[Either[Throwable, List[TreeChange]]](Right(Nil)) {
          case (Right(acc), List(meta, path)) =>
            meta.stripPrefix(":").split(' ').toList match {
              case List(oldMode, newMode, _, newSha1, _) =>
                if (newMode == deletedMode) Right(Delete(path, oldMode) :: acc)
                else Sha1.from(newSha1).map(Write(path, newMode, _) :: acc)
              case _ => Left(new IllegalArgumentException(s"Unexpected diff-tree entry: $meta"))
            }
          case (Right(_), other) =>
            Left(new IllegalArgumentException(s"Unexpected diff-tree output: $other"))
          case (left, _) => left
        }
        .map(_.reverse)
    }
  }
}
