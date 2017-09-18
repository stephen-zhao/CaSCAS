//=============================================================================
// token/SymbolicTokens.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.token

//=============================================================================

import scala.util.matching.Regex

//=============================================================================

//== OPERATOR PLUS TOKEN ======================================================
case class OperatorPlusToken(
  lexeme: String = raw"""+""",
  symbol: Symbol = 'PLUS
) extends Token
  with OperatorTokenLike

object OperatorPlusToken {
  val expected: String = raw"""+"""
  val regex: Regex = raw"""^(\+)(.*)""".r
}

//== OPERATOR MINUS TOKEN =====================================================
case class OperatorMinusToken(
  lexeme: String = raw"-",
  symbol: Symbol = 'MINUS
) extends Token
  with OperatorTokenLike

object OperatorMinusToken {
  val expected: String = raw"""-"""
  val regex: Regex = raw"""^(\-)(.*)""".r
}

//== OPERATOR MULT TOKEN ======================================================
case class OperatorMultToken(
  lexeme: String = raw"""*""",
  symbol: Symbol = 'STAR
) extends Token
  with OperatorTokenLike

object OperatorMultToken {
  val expected: String = raw"""*"""
  val regex: Regex = raw"""^(\*)(.*)""".r
}

//== OPERATOR DIV TOKEN =======================================================
case class OperatorDivToken(
  lexeme: String = raw"""/""",
  symbol: Symbol = 'SLASH
) extends Token
  with OperatorTokenLike

object OperatorDivToken {
  val expected: String = raw"""/"""
  val regex: Regex = raw"""^(\/)(.*)""".r
}

//== OPERATOR POW TOKEN =======================================================
case class OperatorPowToken(
  lexeme: String = raw"""^""",
  symbol: Symbol = 'POW
) extends Token
  with OperatorTokenLike

object OperatorPowToken {
  val expected: String = raw"""^"""
  val regex: Regex = raw"""^(\^)(.*)""".r
}

//== OPERATOR BANG TOKEN ======================================================
case class OperatorBangToken(
  lexeme: String = raw"""!""",
  symbol: Symbol = 'BANG
) extends Token
  with OperatorTokenLike

object OperatorBangToken {
  val expected: String = raw"""!"""
  val regex: Regex = raw"""^(\!)(.*)""".r
}

//== BOOL OPERATOR AND TOKEN ==================================================
case class BoolOperatorAndToken(
  lexeme: String = raw"""/\\""",
  symbol: Symbol = 'AND
) extends Token
  with BoolOperatorTokenLike

object BoolOperatorAndToken{
  val expected: String = raw"""/\\"""
  val regex: Regex = raw"""^(\/\\)(.*)""".r
}

//== BOOL OPERATOR OR TOKEN ===================================================
case class BoolOperatorOrToken(
  lexeme: String = raw"""\\/""",
  symbol: Symbol = 'OR
) extends Token
  with BoolOperatorTokenLike

object BoolOperatorOrToken{
  val expected: String = raw"""\\/"""
  val regex: Regex = raw"""^(\\\/)(.*)""".r
}

//== BOOL OPERATOR NOT TOKEN ==================================================
case class BoolOperatorNotToken(
  lexeme: String = raw"""~""",
  symbol: Symbol = 'NOT
) extends Token
  with BoolOperatorTokenLike

object BoolOperatorNotToken{
  val expected: String = raw"""~"""
  val regex: Regex = raw"""^(~)(.*)""".r
}

//== RELATION LESS EQUAL TOKEN ================================================
case class RelationLessEqualToken(
  lexeme: String = raw"""<=""",
  symbol: Symbol = 'LTE
) extends Token
  with RelationTokenLike

object RelationLessEqualToken {
  val regex: Regex = raw"""^(<=)(.*)""".r
}

//== RELATION GREATER EQUAL TOKEN =============================================
case class RelationGreaterEqualToken(
  lexeme: String = raw""">=""",
  symbol: Symbol = 'GTE
) extends Token
  with RelationTokenLike

object RelationGreaterEqualToken {
  val regex: Regex = raw"""^(>=)(.*)""".r
}

//== RELATION EQUAL TOKEN =====================================================
case class RelationEqualToken(
  lexeme: String = raw"""=""",
  symbol: Symbol = 'EQ
) extends Token
  with RelationTokenLike

object RelationEqualToken {
  val regex: Regex = raw"""^(=)(.*)""".r
}

//== RELATION NOT EQUAL TOKEN =================================================
case class RelationNotEqualToken(
  lexeme: String = raw"""=/=""",
  symbol: Symbol = 'NEQ
) extends Token
  with RelationTokenLike

object RelationNotEqualToken {
  val regex: Regex = raw"""^(=\/=)(.*)""".r
}

//== RELATION LESS TOKEN ======================================================
case class RelationLessToken(
  lexeme: String = raw"""<""",
  symbol: Symbol = 'LT
) extends Token
  with RelationTokenLike

object RelationLessToken {
  val regex: Regex = raw"""^(<)(.*)""".r
}

//== RELATION GREATER TOKEN ===================================================
case class RelationGreaterToken(
  lexeme: String = raw""">""",
  symbol: Symbol = 'GT
) extends Token
  with RelationTokenLike

object RelationGreaterToken {
  val regex: Regex = raw"""^(>)(.*)""".r
}

//== SEMICOLON TOKEN ==========================================================
case class SemicolonToken(
  lexeme: String = raw""";""",
  symbol: Symbol = 'SEMICOLON
) extends Token

object SemicolonToken {
  val regex: Regex = raw"""^(;)(.*)""".r
}

//== COMMA TOKEN ==============================================================
case class CommaToken(
  lexeme: String = raw""",""",
  symbol: Symbol = 'COMMA
) extends Token

object CommaToken {
  val regex: Regex = raw"""^(,)(.*)""".r
}

//== ASSIGNMENT TOKEN =========================================================
case class AssignmentToken(
  lexeme: String = raw""":=""",
  symbol: Symbol = 'ASSIGN
) extends Token

object AssignmentToken {
  val regex: Regex = raw"""^(:=)(.*)""".r
}
