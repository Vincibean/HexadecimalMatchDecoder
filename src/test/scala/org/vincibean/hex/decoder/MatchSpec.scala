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

import cats.implicits._
import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

import scala.io.Source

class MatchSpec extends Specification {

  override def is: SpecStructure =
    s2"""
         Creating a new Match should
            work with self-consistent events $ev1
            not work with not self-consistent events $ev2

      """

  def ev1: MatchResult[Either[DecoderError[_], Match]] =
    newMatch(readLinesFrom("sample1.txt")) must beRight { (x: Match) =>
      x.allEvents must have size 28
    }

  def ev2: MatchResult[Either[DecoderError[_], Match]] =
    newMatch(readLinesFrom("sample2.txt")) must beLeft.like {
      case _: UnorderedElapsedTimeEventsError => true
    }

  def readLinesFrom(sampleName: String): Iterator[String] =
    Source.fromResource(sampleName).getLines

  def newMatch(iter: Iterator[String]): Either[DecoderError[_], Match] =
    for {
      ss <- Right(iter)
      bs <- ss.map(Utils.hexToBinary).toList.sequenceU
      pbs <- bs.map(s => Utils.addPadding(s)).sequenceU
      evs <- pbs.map(MatchEvent.apply).sequenceU
      x <- Match(evs: _*)
    } yield x

}
