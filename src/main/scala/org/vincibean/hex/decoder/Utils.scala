package org.vincibean.hex.decoder

import scala.util.Try
import cats.syntax.either._

import scala.collection.immutable.Seq

object Utils {

  /**
    * Converts a String representing a number in hexadecimal base into a String representing a number in binary base.
    * @param s the String to convert: a String representing a number in hexadecimal base
    * @return either a String representing a number in binary base or an error
    */
  def hexToBinary(s: String): Either[ParseError, String] =
    Try(Integer.decode(s))
      .map(i => i.toInt)
      .map(i => i.toBinaryString)
      .toEither
      .leftMap(t => HexToBinaryError(s, t))

  /**
    * Adds left padding to the given String
    * @param s the String to left pad
    * @param format the format to be used (default: "%31s", i.e. the ensuing String will be 31 characters long)
    * @param toPad the Character that should be used for padding (default: 0)
    * @return either the padded String or an error
    */
  def addPadding(s: String,
                 format: String = """%31s""",
                 toPad: Char = '0'): Either[ParseError, String] =
    Try(format.format(s))
      .map(x => x.replace(' ', toPad))
      .toEither
      .leftMap(t => PaddingError(s, t))

  /**
    * Splits a String in multiple positions.
    * @param str the String to split.
    * @param pos a sequence of positions that will be used for splitting the String
    * @return a sequence of splitted Strings
    */
  def splitsAt(str: String, pos: Seq[Int]): Seq[String] = {
    Try {
      val (head, tail) = pos.foldRight((str, List.empty[String])) {
        case (curr, (s, res)) =>
          val (rest, split) = s.splitAt(curr)
          (rest, split :: res)
      }
      head :: tail
    }.getOrElse(Nil)
  }

  /**
    * Converts a String representing a number in binary base into a String representing a number in decimal base.
    * @param s the String to convert: a String representing a number in binary base
    * @return either a String representing a number in decimal base or an error
    */
  def binaryToDecimal(s: String): Either[ParseError, Int] =
    Try(Integer.parseInt(s, 2)).toEither
      .leftMap(t => BinaryToDecimalError(s, t))

}
