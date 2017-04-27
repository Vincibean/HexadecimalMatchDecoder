/*
 * Copyright (C) 2017  Vincibean <Andre Bessi>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.vincibean.hex.decoder

import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

class MatchEventSpec extends Specification {

  override def is: SpecStructure =
    s2"""
        Calling the Event Binary Data method should
            return the expected MatchEvent $ev1
            fail if a non-binary String is given $ev2
            fail if a String with the wrong length is given $ev3
      """

  def ev1: MatchResult[Either[ParseError, MatchEvent]] = {
    val binaryString = "0000000011110000001000000000010"
    val actual = MatchEvent(binaryString)
    actual must beRight { (x: MatchEvent) =>
      (x.elapsedTime must be equalTo 15) and (x.whoScored must be equalTo 0) and (x.pointsScored must be equalTo 2)
    }
  }

  def ev2: MatchResult[Either[ParseError, MatchEvent]] =
    MatchEvent("123") must beLeft[ParseError]

  def ev3: MatchResult[Either[ParseError, MatchEvent]] =
    MatchEvent("101") must beLeft[ParseError]

}
