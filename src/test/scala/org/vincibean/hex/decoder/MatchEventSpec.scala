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
