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
