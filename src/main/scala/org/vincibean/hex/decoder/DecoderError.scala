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

import scala.collection.immutable.Seq

/**
  * Define an ADT which describes all possible types of error in this library
  * @tparam A the type of the root cause of this error
  */
trait DecoderError[A] {

  /**
    * @return the element that caused this error
    */
  def errorSource: A
}

sealed trait MatchError extends DecoderError[Seq[MatchEvent]]

sealed trait ParseError extends DecoderError[String]

sealed trait HasThrowable extends ParseError {

  /**
    * @return the generated error
    */
  def throwable: Throwable
}

final case class UnorderedElapsedTimeEventsError(errorSource: Seq[MatchEvent])
    extends MatchError

final case class UnorderedTeam1PointsTotalEventsError(
    errorSource: Seq[MatchEvent])
    extends MatchError

final case class UnorderedTeam2PointsTotalEventsError(
    errorSource: Seq[MatchEvent])
    extends MatchError

final case class HexToBinaryError(errorSource: String, throwable: Throwable)
    extends HasThrowable

final case class BinaryToDecimalError(errorSource: String,
                                      throwable: Throwable)
    extends HasThrowable

final case class PaddingError(errorSource: String, throwable: Throwable)
    extends HasThrowable

final case class NonBinaryStringError(errorSource: String) extends ParseError

final case class IncorrectLengthBinaryStringError(errorSource: String)
    extends ParseError
