package org.cascas_project.cascas.tokens

import scala.util.matching._

trait Tokenizeable {
  def lexeme: String
  def length(): Int = {
    lexeme.length
  }
}

abstract class Token
extends Tokenizeable
{
  def lexeme: String
}

case class IntegerToken(
  val lexeme: String
) extends Token

object IntegerToken {
  val regex: UnanchoredRegex = raw"^(([0-9]_?)*[0-9])".r.unanchored
}

case class DecimalToken(
  val lexeme: String
) extends Token

object DecimalToken {
  val regex: UnanchoredRegex = raw"^(([0-9]_?)*[0-9]?\.[0-9](_?[0-9])*)".r.unanchored
}

/*
object OperatorType extends Enumeration {
  type OperatorType = Value
  val OperatorPlus  = Value
  val OperatorMinus = Value
  val OperatorMult  = Value
  val OperatorDiv   = Value
  val OperatorPow   = Value
  val OperatorBang  = Value
}
import OperatorType._

case class OperatorToken(
  lexeme: String,
  whichOperator: OperatorType
) extends Token

object OperatorToken {
  val regex: UnanchoredRegex = raw"^([\+\-\*\/\^\!])".r.unanchored

  def expectedLexeme(whichOperator: OperatorType) : String = whichOperator match {
    case OperatorPlus   => "+"
    case OperatorMinus  => "-"
    case OperatorMult   => "*"
    case OperatorDiv    => "/"
    case OperatorPow    => "^"
    case OperatorBang   => "!"
  }
}
*/

case class OperatorPlusToken(
  val lexeme: String = raw"+"
) extends Token

object OperatorPlusToken {
  val expected: String = raw"+"
  val regex: UnanchoredRegex = raw"^(\+)".r.unanchored
}

case class OperatorMinusToken(
  val lexeme: String = raw"-"
) extends Token

object OperatorMinusToken {
  val expected: String = raw"-"
  val regex: UnanchoredRegex = raw"^(\-)".r.unanchored
}

case class OperatorMultToken(
  val lexeme: String = raw"*"
) extends Token

object OperatorMultToken {
  val expected: String = raw"*"
  val regex: UnanchoredRegex = raw"^(\*)".r.unanchored
}

case class OperatorDivToken(
  val lexeme: String = raw"/"
) extends Token

object OperatorDivToken {
  val expected: String = raw"/"
  val regex: UnanchoredRegex = raw"^(\/)".r.unanchored
}

case class OperatorPowToken(
  val lexeme: String = raw"^"
) extends Token

object OperatorPowToken {
  val expected: String = raw"^"
  val regex: UnanchoredRegex = raw"^(\^)".r.unanchored
}

case class OperatorBangToken(
  val lexeme: String = raw"!"
) extends Token

object OperatorBangToken {
  val expected: String = raw"!"
  val regex: UnanchoredRegex = raw"^(\!)".r.unanchored
}

/*
object BracketEndType extends Enumeration {
  type BracketEndType = Value
  val BracketLeft     = Value
  val BracketRight    = Value
}
import BracketEndType._

trait BracketEndable {
  def whichEnd: BracketEndType
}

case class RoundBracketToken(
  lexeme: String,
  whichEnd: BracketEndType
) extends Token
  with BracketEndable

object RoundBracketToken {
  val regex: UnanchoredRegex = raw"^([\(\)])".r.unanchored

  def expectedLexeme(whichEnd: BracketEndType): String = whichEnd match {
    case BracketLeft  => "("
    case BracketRight => ")"
  }
}

case class SquareBracketToken(
  lexeme: String,
  whichEnd: BracketEndType
) extends Token
  with BracketEndable

object SquareBracketToken {
  val regex: UnanchoredRegex = raw"^([\[\]])".r.unanchored

  def expectedLexeme(whichEnd: BracketEndType): String = whichEnd match {
    case BracketLeft  => "["
    case BracketRight => "]"
  }
}

case class CurlyBracketToken(
  lexeme: String,
  whichEnd: BracketEndType
) extends Token
  with BracketEndable

object CurlyBracketToken {
  val regex: UnanchoredRegex = raw"^([\{\}])".r.unanchored

  def expectedLexeme(whichEnd: BracketEndType): String = whichEnd match {
    case BracketLeft  => "{"
    case BracketRight => "}"
  }
}
*/

case class LeftRoundBracketToken(
  val lexeme: String = raw"("
) extends Token

object LeftRoundBracketToken {
  val expected: String = raw"("
  val regex: UnanchoredRegex = raw"^(\()".r.unanchored
}

case class RightRoundBracketToken(
  val lexeme: String = raw")"
) extends Token

object RightRoundBracketToken {
  val expected: String = raw")"
  val regex: UnanchoredRegex = raw"^(\))".r.unanchored
}

case class LeftSquareBracketToken(
  val lexeme: String = raw"["
) extends Token

object LeftSquareBracketToken {
  val expected: String = raw"["
  val regex: UnanchoredRegex = raw"^(\[)".r.unanchored
}

case class RightSquareBracketToken(
  val lexeme: String = raw"]"
) extends Token

object RightSquareBracketToken {
  val expected: String = raw"]"
  val regex: UnanchoredRegex = raw"^(\])".r.unanchored
}

case class LeftCurlyBracketToken(
  val lexeme: String = raw"{"
) extends Token

object LeftCurlyBracketToken {
  val expected: String = raw"{"
  val regex: UnanchoredRegex = raw"^(\{)".r.unanchored
}

case class RightCurlyBracketToken(
  val lexeme: String = raw"}"
) extends Token

object RightCurlyBracketToken {
  val expected: String = raw"}"
  val regex: UnanchoredRegex = raw"^(\})".r.unanchored
}

case class WhitespaceToken(
  val lexeme: String
) extends Token

object WhitespaceToken {
  val regex: UnanchoredRegex = raw"^(\s+)".r.unanchored
}

case class OtherToken(
  val lexeme: String
) extends Token

object OtherToken {
  val regex: UnanchoredRegex = raw"^([A-Za-z_](\w)*)".r.unanchored
}

