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
  def symbol: Symbol
}

trait NumberTokenLike {
}

trait OperatorTokenLike {
}

trait BracketTokenLike {
}

trait RelationTokenLike {
}

//trait KeywordTokenLike {
//}

case class IntegerToken(
  val lexeme: String,
  val symbol: Symbol = 'INT
) extends Token
  with NumberTokenLike

object IntegerToken {
  val regex: UnanchoredRegex = raw"""^(([0-9]_?)*[0-9])""".r.unanchored
}

case class DecimalToken(
  val lexeme: String,
  val symbol: Symbol = 'FLOAT
) extends Token
  with NumberTokenLike

object DecimalToken {
  val regex: UnanchoredRegex = raw"""^(([0-9]_?)*[0-9]?\.[0-9](_?[0-9])*)""".r.unanchored
}

case class OperatorPlusToken(
  val lexeme: String = raw"""+""",
  val symbol: Symbol = 'PLUS
) extends Token
  with OperatorTokenLike

object OperatorPlusToken {
  val expected: String = raw"""+"""
  val regex: UnanchoredRegex = raw"""^(\+)""".r.unanchored
}

case class OperatorMinusToken(
  val lexeme: String = raw"-",
  val symbol: Symbol = 'MINUS
) extends Token
  with OperatorTokenLike

object OperatorMinusToken {
  val expected: String = raw"""-"""
  val regex: UnanchoredRegex = raw"""^(\-)""".r.unanchored
}

case class OperatorMultToken(
  val lexeme: String = raw"""*""",
  val symbol: Symbol = 'STAR
) extends Token
  with OperatorTokenLike

object OperatorMultToken {
  val expected: String = raw"""*"""
  val regex: UnanchoredRegex = raw"""^(\*)""".r.unanchored
}

case class OperatorDivToken(
  val lexeme: String = raw"""/""",
  val symbol: Symbol = 'SLASH
) extends Token
  with OperatorTokenLike

object OperatorDivToken {
  val expected: String = raw"""/"""
  val regex: UnanchoredRegex = raw"""^(\/)""".r.unanchored
}

case class OperatorPowToken(
  val lexeme: String = raw"""^""",
  val symbol: Symbol = 'POW
) extends Token
  with OperatorTokenLike

object OperatorPowToken {
  val expected: String = raw"""^"""
  val regex: UnanchoredRegex = raw"""^(\^)""".r.unanchored
}

case class OperatorBangToken(
  val lexeme: String = raw"""!""",
  val symbol: Symbol = 'BANG
) extends Token
  with OperatorTokenLike

object OperatorBangToken {
  val expected: String = raw"""!"""
  val regex: UnanchoredRegex = raw"""^(\!)""".r.unanchored
}

case class LeftRoundBracketToken(
  val lexeme: String = raw"""(""",
  val symbol: Symbol = 'LRBRACK
) extends Token
  with BracketTokenLike

object LeftRoundBracketToken {
  val expected: String = raw"""("""
  val regex: UnanchoredRegex = raw"""^(\()""".r.unanchored
}

case class RightRoundBracketToken(
  val lexeme: String = raw""")""",
  val symbol: Symbol = 'RRBRACK
) extends Token
  with BracketTokenLike

object RightRoundBracketToken {
  val expected: String = raw""")"""
  val regex: UnanchoredRegex = raw"""^(\))""".r.unanchored
}

case class LeftSquareBracketToken(
  val lexeme: String = raw"""[""",
  val symbol: Symbol = 'LSBRACK
) extends Token
  with BracketTokenLike

object LeftSquareBracketToken {
  val expected: String = raw"""["""
  val regex: UnanchoredRegex = raw"""^(\[)""".r.unanchored
}

case class RightSquareBracketToken(
  val lexeme: String = raw"""]""",
  val symbol: Symbol = 'RSBRACK
) extends Token
  with BracketTokenLike

object RightSquareBracketToken {
  val expected: String = raw"""]"""
  val regex: UnanchoredRegex = raw"""^(\])""".r.unanchored
}

case class LeftCurlyBracketToken(
  val lexeme: String = raw"""{""",
  val symbol: Symbol = 'LCBRACK
) extends Token
  with BracketTokenLike

object LeftCurlyBracketToken {
  val expected: String = raw"""{"""
  val regex: UnanchoredRegex = raw"""^(\{)""".r.unanchored
}

case class RightCurlyBracketToken(
  val lexeme: String = raw"""}""",
  val symbol: Symbol = 'RCBRACK
) extends Token
  with BracketTokenLike

object RightCurlyBracketToken {
  val expected: String = raw"""}"""
  val regex: UnanchoredRegex = raw"""^(\})""".r.unanchored
}

case class RelationLessEqualToken(
  val lexeme: String = raw"""<=""",
  val symbol: Symbol = 'LE
) extends Token
  with RelationTokenLike

object RelationLessEqualToken {
  val regex: UnanchoredRegex = raw"""^(<=)""".r.unanchored
}

case class RelationGreaterEqualToken(
  val lexeme: String = raw""">=""",
  val symbol: Symbol = 'GE
) extends Token
  with RelationTokenLike

object RelationGreaterEqualToken {
  val regex: UnanchoredRegex = raw"""^(>=)""".r.unanchored
}

case class RelationEqualToken(
  val lexeme: String = raw"""=""",
  val symbol: Symbol = 'EQ
) extends Token
  with RelationTokenLike

object RelationEqualToken {
  val regex: UnanchoredRegex = raw"""^(=)""".r.unanchored
}

case class RelationNotEqualToken(
  val lexeme: String = raw"""!=""",
  val symbol: Symbol = 'NEQ
) extends Token
  with RelationTokenLike

object RelationNotEqualToken {
  val regex: UnanchoredRegex = raw"""^(!=)""".r.unanchored
}

case class RelationLessToken(
  val lexeme: String = raw"""<""",
  val symbol: Symbol = 'LT
) extends Token
  with RelationTokenLike

object RelationLessToken {
  val regex: UnanchoredRegex = raw"""^(<)""".r.unanchored
}

case class RelationGreaterToken(
  val lexeme: String = raw""">""",
  val symbol: Symbol = 'GT
) extends Token
  with RelationTokenLike

object RelationGreaterToken {
  val regex: UnanchoredRegex = raw"""^(>)""".r.unanchored
}

case class WhitespaceToken(
  val lexeme: String,
  val symbol: Symbol = 'WS
) extends Token

object WhitespaceToken {
  val regex: UnanchoredRegex = raw"""^(\s+)""".r.unanchored
}

case class CommaToken(
  val lexeme: String = raw""",""",
  val symbol: Symbol = 'COMMA
) extends Token

object CommaToken {
  val regex: UnanchoredRegex = raw"""^(,)""".r.unanchored
}

case class AssignmentToken(
  val lexeme: String = raw""":=""",
  val symbol: Symbol = 'ASSIGN
) extends Token

object AssignmentToken {
  val regex: UnanchoredRegex = raw"""^(:=)""".r.unanchored
}

case class CommentToken(
  val lexeme: String,
  val symbol: Symbol = 'COMMENT
) extends Token

object CommentToken {
  val regex: UnanchoredRegex = raw"""^(;.*)$$""".r.unanchored
}

case class WordToken(
  val lexeme: String,
  val symbol: Symbol = 'WORD
) extends Token

object WordToken {
  val regex: UnanchoredRegex = raw"""^([A-Za-z_](\w)*)""".r.unanchored
}

//case class KeywordDefToken(
//  val lexeme: String = raw"""def"""
//) extends Token
//  with KeywordTokenLike
//
//object KeywordDefToken {
//  val expected: String = raw"""def"""
//}
