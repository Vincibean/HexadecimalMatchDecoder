package org.vincibean.hex.decoder

import org.specs2.Specification
import org.specs2.matcher.MatchResult

class UtilsSpec extends Specification {

  override def is = s2"""
              Padding a String which is the output of the Hex to Binary method should
                  return the expected result $ev1
              Calling the Hex to Binary method should
                  return the binary representation of that number $ev2
                  return a Left if an empty String is given $ev3
                  return a Left if a null is given $ev4
              Calling the Add Padding method should
                  return a properly padded String $ev5
                  pad even an empty String $ev6
                  pad even a null String $ev7
              Calling the Splits At method should
                  return a list having the expected size and containing the expected elements $ev8
                  return a list of empty Strings if an empty String $ev9
                  return Nil if a null String is given $ev10
              Calling the Binary to Decimal method should
                  return a integer with a base of 10 $ev11
                  return a Left if a binary isn't given $ev12
                  return a Left if an empty String is given $ev13
                  return a Left if a null is given $ev14
    """

  def ev1: MatchResult[Either[ParseError, String]] = {
    val actual =
      Utils.hexToBinary("0x781002").flatMap(x => Utils.addPadding(x))
    val expected = "0000000011110000001000000000010"
    actual must beRight { (x: String) =>
      x must be equalTo expected
    }
  }

  def ev2: MatchResult[Either[ParseError, String]] = {
    val actual = Utils.hexToBinary("0x781002")
    val expected = "11110000001000000000010"
    actual must beRight { (x: String) =>
      x must be equalTo expected
    }
  }

  def ev3: MatchResult[Either[ParseError, String]] = {
    val res = Utils.hexToBinary("")
    res must beLeft[ParseError]
  }

  def ev4: MatchResult[Either[ParseError, String]] = {
    val res = Utils.hexToBinary(null)
    res must beLeft[ParseError]
  }

  def ev5: MatchResult[Either[ParseError, String]] = {
    val res = Utils.addPadding("MyTestString", "%15s")
    res must beRight { (x: String) =>
      x must be equalTo "000MyTestString"
    }
  }

  def ev6: MatchResult[Either[ParseError, String]] = {
    val res = Utils.addPadding("", "%3s")
    res must beRight { (x: String) =>
      x must be equalTo "000"
    }
  }

  def ev7: MatchResult[Either[ParseError, String]] = {
    val res = Utils.addPadding(null, "%5s")
    res must beRight { (x: String) =>
      x must be equalTo "0null"
    }
  }

  def ev8: MatchResult[Seq[String]] = {
    val res = Utils.splitsAt("12345", List(1, 2, 3))
    (res must haveSize(4)) and (res must contain(exactly("1", "2", "3", "45")))
  }

  def ev9: MatchResult[Seq[String]] = {
    val res = Utils.splitsAt("", List(1, 2, 3))
    (res must haveSize(4)) and (res must containTheSameElementsAs(
      Seq("", "", "", "")))
  }

  def ev10: MatchResult[Seq[String]] =
    Utils.splitsAt(null, List(1, 2, 3)) must beEmpty

  def ev11: MatchResult[Either[ParseError, Int]] = {
    val actual = Utils.binaryToDecimal("011")
    val expected = 3
    actual must beRight { (x: Int) =>
      x must be equalTo expected
    }
  }

  def ev12: MatchResult[Either[ParseError, Int]] =
    Utils.binaryToDecimal("123") must beLeft[ParseError]

  def ev13: MatchResult[Either[ParseError, Int]] =
    Utils.binaryToDecimal("") must beLeft[ParseError]

  def ev14: MatchResult[Either[ParseError, Int]] =
    Utils.binaryToDecimal(null) must beLeft[ParseError]

}
