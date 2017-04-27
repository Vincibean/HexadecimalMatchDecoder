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

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import scala.collection.{SeqLike, mutable}

object Match {

  def apply(events: MatchEvent*): Either[MatchError, Match] =
    if (events.map(_.elapsedTime).sorted != events.map(_.elapsedTime))
      Left(UnorderedElapsedTimeEventsError(events.toVector))
    else if (events.map(_.team1PointsTotal).sorted != events.map(
               _.team1PointsTotal))
      Left(UnorderedTeam1PointsTotalEventsError(events.toVector))
    else if (events.map(_.team1PointsTotal).sorted != events.map(
               _.team1PointsTotal))
      Left(UnorderedTeam2PointsTotalEventsError(events.toVector))
    else
      Right(new Match(events.toVector))

  implicit def canBuildFrom = new CanBuildFrom[Match, MatchEvent, Match] {
    override def apply(from: Match): mutable.Builder[MatchEvent, Match] =
      apply()

    override def apply(): mutable.Builder[MatchEvent, Match] =
      ListBuffer[MatchEvent]().mapResult(new Match(_))
  }

}

class Match private (private val events: Seq[MatchEvent])
    extends Seq[MatchEvent]
    with SeqLike[MatchEvent, Match] {

  override def length: Int = events.length

  override def apply(idx: Int): MatchEvent = events.apply(idx)

  override def iterator: Iterator[MatchEvent] = events.iterator

  /**
    * Just an alias for method last
    * @return the last MatchEvent of this Match
    */
  def lastEvent: MatchEvent = last

  /**
    * An error-aware alias for method dropRigh()
    * @param n The number of elements to take
    * @return either a Match containing the last n events, or nothing
    */
  def lastEvents(n: Int): Option[Match] =
    if (n > length) None else Option(dropRight(n))

  /**
    * Get all the MatchEvents of this Match.
    * N.B.: it is advisable to use the Match directly, having it all the monadic method of a sequence of MatchEvents
    * @return a Sequence containing all the MatchEvents of this Match.
    */
  def allEvents: Seq[MatchEvent] = events

  override def equals(other: Any): Boolean =
    other match {
      case that: Match => (this eq that) || (this.events == that.events)
      case _ => false
    }

  override def hashCode: Int = events.hashCode

  override protected[this] def newBuilder: mutable.Builder[MatchEvent, Match] =
    Match.canBuildFrom()

}
