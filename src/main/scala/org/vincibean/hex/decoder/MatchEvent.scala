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

object MatchEvent {

  /**
    * The default positions to be used for splitting a binary String into list of MatchEvent parameters.
    */
  val splitPositions = List(2, 3, 11, 19, 31)

  def apply(s: BinaryString): Either[ParseError, MatchEvent] =
    if (s.size != 31)
      Left(IncorrectLengthBinaryStringError(s))
    else if (s.exists(c => c != '0' && c != '1'))
      Left(NonBinaryStringError(s))
    else
      (toMatchEvent _).tupled(eventBinaryData(s))

  private def eventBinaryData(
      bs: BinaryString,
      positions: Seq[Int] = splitPositions): (PointsScored,
                                              WhoScored,
                                              Team2PointsTotal,
                                              Team1PointsTotal,
                                              ElapsedTime) = {
    val x = Utils.splitsAt(bs.reverse, positions).map(_.reverse)
    (x(0), x(1), x(2), x(3), x(4))
  }

  private def toMatchEvent(
      pointsScored: PointsScored,
      whoScored: WhoScored,
      team2PointsTotal: Team2PointsTotal,
      team1PointsTotal: Team1PointsTotal,
      elapsedTime: ElapsedTime): Either[ParseError, MatchEvent] =
    for {
      points <- Utils.binaryToDecimal(pointsScored)
      who <- Utils.binaryToDecimal(whoScored)
      tot2 <- Utils.binaryToDecimal(team2PointsTotal)
      tot1 <- Utils.binaryToDecimal(team1PointsTotal)
      t <- Utils.binaryToDecimal(elapsedTime)
    } yield new MatchEvent(points, who, tot2, tot1, t)

}

class MatchEvent private (val pointsScored: Int,
                          val whoScored: Int,
                          val team2PointsTotal: Int,
                          val team1PointsTotal: Int,
                          val elapsedTime: Int) {

  def copy(s: BinaryString): Either[ParseError, MatchEvent] = MatchEvent(s)

  override def equals(other: Any): Boolean =
    other match {
      case that: MatchEvent =>
        (this eq that) || (this.pointsScored == that.pointsScored && this.whoScored == that.whoScored && this.team2PointsTotal == that.team2PointsTotal && this.team1PointsTotal == that.team1PointsTotal && this.elapsedTime == that.elapsedTime)
      case _ => false
    }

  override def hashCode: Int =
    (pointsScored, whoScored, team2PointsTotal, team1PointsTotal, elapsedTime)
      .hashCode()

}
