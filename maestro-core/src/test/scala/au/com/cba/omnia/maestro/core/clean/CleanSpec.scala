//   Copyright 2017 Commonwealth Bank of Australia
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
package clean

import org.scalacheck.Prop

import au.com.cba.omnia.maestro.core.data.Field

object CleanSpec extends test.Spec { def is = s2"""

Clean properties
================

  US-ASCII is handled the same by posixDefault and unicodeDefault      $ascii
  default trims the same way it always has (no regression)             $default
  posixDefault trims to POSIX US-ASCII Printable chars only            $posixDefault
  unicodeDefault trims to Unicode TR#18 Annex C Printable chars only   $unicodeDefault

"""

  val f = Field("dummy", (s: String) => s)

  val allAscii = (0 to 127).map(_.toChar).mkString

  val posixPrintable =
    " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

  val isPosixPrintable = posixPrintable.toSet

  def isUnicodePrintable(c: Char): Boolean =
    c.toString.matches("(?U)[\\p{Graph}\\p{Blank}&&[^\\p{Cntrl}]]")

  def ascii = {
    val posixResult = Clean.posixDefault.run(f, allAscii)
    val unicodeResult = Clean.unicodeDefault.run(f, allAscii)
    val expectedResult = posixPrintable.mkString.trim
    // three-way comparison:
    Set(posixResult, unicodeResult) must_== Set(expectedResult)
  }

  def default = {
    val testRange = (0 to 255).map(_.toChar).mkString
    val oldDefault = Clean((_, data) => data.trim.replaceAll("[^\\p{Print}]", "")) // traditional implementation
    Clean.default.run(f, testRange) must_== oldDefault.run(f, testRange)
  }

  def posixDefault = Prop.forAll { (str: String) =>
    Clean.posixDefault.run(f, str) must_== str.trim.filter(isPosixPrintable)
  }

  def unicodeDefault = Prop.forAll { (str: String) =>
    Clean.unicodeDefault.run(f, str) must_== str.trim.filter(isUnicodePrintable)
  }
}
