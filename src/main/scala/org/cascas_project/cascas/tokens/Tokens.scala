package org.cascas_project.cascas.tokens

import scala.util.matching._

abstract class Token
{
  def lexeme: String
  def symbol: Symbol
  def length(): Int = {
    lexeme.length
  }
}

trait NumberTokenLike {
//  val wholePart: String
//  val fractionalPart: String
}

trait OperatorTokenLike {
}

trait BoolOperatorTokenLike{
}

trait BracketTokenLike {
}

trait RelationTokenLike {
}

trait WhitespaceTokenLike {
}

trait KeywordTokenLike {
}

//trait KeywordTokenLike {
//}

case class IntegerToken(
  val lexeme: String,
  val symbol: Symbol = 'INT
) extends Token
  with NumberTokenLike

object IntegerToken {
  val regex: Regex = raw"""^((?:[0-9]_?)*[0-9])(.*)""".r
}

case class DecimalToken(
  val lexeme: String,
  val symbol: Symbol = 'FLOAT
) extends Token
  with NumberTokenLike

object DecimalToken {
  val regex: Regex = raw"""^((?:[0-9]_?)*[0-9]?\.[0-9](?:_?[0-9])*)(.*)""".r
}

case class OperatorPlusToken(
  val lexeme: String = raw"""+""",
  val symbol: Symbol = 'PLUS
) extends Token
  with OperatorTokenLike

object OperatorPlusToken {
  val expected: String = raw"""+"""
  val regex: Regex = raw"""^(\+)(.*)""".r
}

case class OperatorMinusToken(
  val lexeme: String = raw"-",
  val symbol: Symbol = 'MINUS
) extends Token
  with OperatorTokenLike

object OperatorMinusToken {
  val expected: String = raw"""-"""
  val regex: Regex = raw"""^(\-)(.*)""".r
}

case class OperatorMultToken(
  val lexeme: String = raw"""*""",
  val symbol: Symbol = 'STAR
) extends Token
  with OperatorTokenLike

object OperatorMultToken {
  val expected: String = raw"""*"""
  val regex: Regex = raw"""^(\*)(.*)""".r
}

case class OperatorDivToken(
  val lexeme: String = raw"""/""",
  val symbol: Symbol = 'SLASH
) extends Token
  with OperatorTokenLike

object OperatorDivToken {
  val expected: String = raw"""/"""
  val regex: Regex = raw"""^(\/)(.*)""".r
}

case class OperatorPowToken(
  val lexeme: String = raw"""^""",
  val symbol: Symbol = 'POW
) extends Token
  with OperatorTokenLike

object OperatorPowToken {
  val expected: String = raw"""^"""
  val regex: Regex = raw"""^(\^)(.*)""".r
}

case class OperatorBangToken(
  val lexeme: String = raw"""!""",
  val symbol: Symbol = 'BANG
) extends Token
  with OperatorTokenLike

object OperatorBangToken {
  val expected: String = raw"""!"""
  val regex: Regex = raw"""^(\!)(.*)""".r
}

case class BoolOperatorAndToken(
  val lexeme: String = raw"""/\\""",
  val symbol: Symbol = 'AND
) extends Token
  with BoolOperatorTokenLike

object BoolOperatorAndToken{
  val expected: String = raw"""/\\"""
  val regex: Regex = raw"""^(\/\\)(.*)""".r
}

case class BoolOperatorOrToken(
  val lexeme: String = raw"""\\/""",
  val symbol: Symbol = 'OR
) extends Token
  with BoolOperatorTokenLike

object BoolOperatorOrToken{
  val expected: String = raw"""\\/"""
  val regex: Regex = raw"""^(\\\/)(.*)""".r
}

case class BoolOperatorNotToken(
  val lexeme: String = raw"""~""",
  val symbol: Symbol = 'NOT
) extends Token
  with BoolOperatorTokenLike

object BoolOperatorNotToken{
  val expected: String = raw"""~"""
  val regex: Regex = raw"""^(~)(.*)""".r
}
case class LeftRoundBracketToken(
  val lexeme: String = raw"""(""",
  val symbol: Symbol = 'LRBRACK
) extends Token
  with BracketTokenLike

object LeftRoundBracketToken {
  val expected: String = raw"""("""
  val regex: Regex = raw"""^(\()(.*)""".r
}

case class RightRoundBracketToken(
  val lexeme: String = raw""")""",
  val symbol: Symbol = 'RRBRACK
) extends Token
  with BracketTokenLike

object RightRoundBracketToken {
  val expected: String = raw""")"""
  val regex: Regex = raw"""^(\))(.*)""".r
}

case class LeftSquareBracketToken(
  val lexeme: String = raw"""[""",
  val symbol: Symbol = 'LSBRACK
) extends Token
  with BracketTokenLike

object LeftSquareBracketToken {
  val expected: String = raw"""["""
  val regex: Regex = raw"""^(\[)(.*)""".r
}

case class RightSquareBracketToken(
  val lexeme: String = raw"""]""",
  val symbol: Symbol = 'RSBRACK
) extends Token
  with BracketTokenLike

object RightSquareBracketToken {
  val expected: String = raw"""]"""
  val regex: Regex = raw"""^(\])(.*)""".r
}

case class LeftCurlyBracketToken(
  val lexeme: String = raw"""{""",
  val symbol: Symbol = 'LCBRACK
) extends Token
  with BracketTokenLike

object LeftCurlyBracketToken {
  val expected: String = raw"""{"""
  val regex: Regex = raw"""^(\{)(.*)""".r
}

case class RightCurlyBracketToken(
  val lexeme: String = raw"""}""",
  val symbol: Symbol = 'RCBRACK
) extends Token
  with BracketTokenLike

object RightCurlyBracketToken {
  val expected: String = raw"""}"""
  val regex: Regex = raw"""^(\})(.*)""".r
}

case class RelationLessEqualToken(
  val lexeme: String = raw"""<=""",
  val symbol: Symbol = 'LE
) extends Token
  with RelationTokenLike

object RelationLessEqualToken {
  val regex: Regex = raw"""^(<=)(.*)""".r
}

case class RelationGreaterEqualToken(
  val lexeme: String = raw""">=""",
  val symbol: Symbol = 'GE
) extends Token
  with RelationTokenLike

object RelationGreaterEqualToken {
  val regex: Regex = raw"""^(>=)(.*)""".r
}

case class RelationEqualToken(
  val lexeme: String = raw"""=""",
  val symbol: Symbol = 'EQ
) extends Token
  with RelationTokenLike

object RelationEqualToken {
  val regex: Regex = raw"""^(=)(.*)""".r
}

case class RelationNotEqualToken(
  val lexeme: String = raw"""!=""",
  val symbol: Symbol = 'NEQ
) extends Token
  with RelationTokenLike

object RelationNotEqualToken {
  val regex: Regex = raw"""^(!=)(.*)""".r
}

case class RelationLessToken(
  val lexeme: String = raw"""<""",
  val symbol: Symbol = 'LT
) extends Token
  with RelationTokenLike

object RelationLessToken {
  val regex: Regex = raw"""^(<)(.*)""".r
}

case class RelationGreaterToken(
  val lexeme: String = raw""">""",
  val symbol: Symbol = 'GT
) extends Token
  with RelationTokenLike

object RelationGreaterToken {
  val regex: Regex = raw"""^(>)(.*)""".r
}

case class WhitespaceToken(
  val lexeme: String,
  val symbol: Symbol = 'WHITESPACE
) extends Token
  with WhitespaceTokenLike

object WhitespaceToken {
  val regex: Regex = raw"""^(\s+)(.*)""".r
}

case class SemicolonToken(
  val lexeme: String = raw""";""",
  val symbol: Symbol = 'SEMICOLON
) extends Token

object SemicolonToken {
  val regex: Regex = raw"""^(;)(.*)""".r
}

case class CommaToken(
  val lexeme: String = raw""",""",
  val symbol: Symbol = 'COMMA
) extends Token

object CommaToken {
  val regex: Regex = raw"""^(,)(.*)""".r
}

case class AssignmentToken(
  val lexeme: String = raw""":=""",
  val symbol: Symbol = 'ASSIGN
) extends Token

object AssignmentToken {
  val regex: Regex = raw"""^(:=)(.*)""".r
}

case class CommentToken(
  val lexeme: String,
  val symbol: Symbol = 'COMMENT
) extends Token

object CommentToken {
  val regex: Regex = raw"""^(#.*)$$(.*)""".r
}

case class WordToken(
  val lexeme: String,
  val symbol: Symbol = 'WORD
) extends Token

object WordToken {
  val regex: Regex = raw"""^([A-Za-z_\u03bb](?:\w)*)(.*)""".r
}

//case class EndOfLineToken(
//  val lexeme: String = "\n",
//  val symbol: Symbol = 'EOL
//) extends Token
//
//object EndOfLineToken {
//}

case class KeywordLambdaToken(
  val lexeme: String,
  val symbol: Symbol = 'LAMBDA
) extends Token
  with KeywordTokenLike

object KeywordLambdaToken {
  val regex: Regex = (raw"""^(lambda|""" + '\u03bb'.toString + raw""")(.*)""").r
}

case class KeywordLetToken(
  val lexeme: String = raw"""let""",
  val symbol: Symbol = 'LET
) extends Token
  with KeywordTokenLike

object KeywordLetToken {
  val expected: String = raw"""let"""
}

case class KeywordIfToken(
  val lexeme: String = raw"""if""",
  val symbol: Symbol = 'IF
) extends Token
  with KeywordTokenLike

object KeywordIfToken {
  val expected: String = raw"""if"""
}

case class KeywordElsifToken(
  val lexeme: String = raw"""elsif""",
  val symbol: Symbol = 'ELSIF
) extends Token
  with KeywordTokenLike

object KeywordElsifToken {
  val expected: String = raw"""elsif"""
}

case class KeywordElseToken(
  val lexeme: String = raw"""else""",
  val symbol: Symbol = 'ELSE
) extends Token
  with KeywordTokenLike

object KeywordElseToken {
  val expected: String = raw"""else"""
}

case class KeywordWhileToken(
  val lexeme: String = raw"""while""",
  val symbol: Symbol = 'WHILE
) extends Token
  with KeywordTokenLike

object KeywordWhileToken {
  val expected: String = raw"""while"""
}

case class KeywordForToken(
  val lexeme: String = raw"""for""",
  val symbol: Symbol = 'FOR
) extends Token
  with KeywordTokenLike

object KeywordForToken {
  val expected: String = raw"""for"""
}

case class KeywordInToken(
  val lexeme: String = raw"""in""",
  val symbol: Symbol = 'IN
) extends Token
  with KeywordTokenLike

object KeywordInToken {
  val expected: String = raw"""in"""
}
