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
