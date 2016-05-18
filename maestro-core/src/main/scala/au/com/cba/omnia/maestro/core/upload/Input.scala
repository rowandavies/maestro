//   Copyright 2014 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

package au.com.cba.omnia.maestro.core
package upload

import java.io.File

import scala.util.matching.Regex

import org.joda.time.{DateTime, DateTimeZone}

import scalaz._, Scalaz._

import au.com.cba.omnia.omnitool.Result

/** A control file, not to be loaded into HDFS */
case class ControlFile(file: File)

/** A control file, not to be loaded into HDFS, with timestamp */
case class ControlFileTimestamped(file: File, val timestamp: DateTime) {
  def toControlFile = ControlFile(file)
}

/** A file to be uploaded, with associated business date directory */
case class DataFile(file: String, parsedDate: String)

/** A file to be uploaded, with associated business date directory and date-time */
case class DataFileTimestamped(file: String, parsedDate: String, timestamp: DateTime) {
  def toDataFile = DataFile(file, parsedDate)
}

/** Files found by [[Input.findFiles]] */
case class InputFiles(controlFiles: List[ControlFile], dataFiles: List[DataFile])

case class InputFilesTimestamped(controlFiles: List[ControlFileTimestamped], dataFiles: List[DataFileTimestamped]) {
  def toInputFiles = InputFiles(controlFiles.map(_.toControlFile), dataFiles.map(_.toDataFile))
}

/** Factory for `Input`s */
object Input {

  /**
    * Find files from a directory matching a given file pattern, with `DateTime`s returned.
    *
    * See the description of the file pattern at [[au.com.cba.omnia.maestro.core.task.Upload]]
    */
  def findFiles(
    sourceDir: String, tablename: String, filePattern: String, controlPattern: Regex, timeZone: DateTimeZone
  ): Result[InputFilesTimestamped] = for {
    matcher <- InputParsers.forPattern(timeZone)(tablename, filePattern)
    files   <- fromNullable((new File(sourceDir)).listFiles, s"${sourceDir} does not exist").map(_.toList)
    results <- files traverse (file => matcher(file.getName))
  } yield {
    val matches      = (files zip results) collect { case (file, Match(dirs, stamp)) => (file, dirs, stamp) }
    val (ctrl, data) = matches partition { case (file, _, _) => isControl(file, controlPattern) }
    val controls     = ctrl map { case (file, _, stamp)      => new ControlFileTimestamped(file, stamp) }
    val dataFiles    = data map { case (file, dirs, stamp)   =>
      new DataFileTimestamped(file.toString, dirs mkString File.separator, stamp)
    }
    new InputFilesTimestamped(controls, dataFiles)
  }

  def isControl(file: File, controlPattern: Regex) =
    controlPattern.unapplySeq(file.getName).isDefined

  def fromNullable[A](v: A, msg: String) =
    if (v == null) Result.fail(msg) else Result.ok(v)
}
