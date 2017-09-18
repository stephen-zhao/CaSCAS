//=============================================================================
// token/BracketTokens.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.token

//=============================================================================

import scala.util.matching.Regex

//=============================================================================

//== LEFT ROUND BRACKET TOKEN =================================================
case class LeftRoundBracketToken(
  lexeme: String = raw"""(""",
  symbol: Symbol = 'LRBRACK
) extends Token
  with BracketTokenLike

object LeftRoundBracketToken {
  val expected: String = raw"""("""
  val regex: Regex = raw"""^(\()(.*)""".r
}

//== RIGHT ROUND BRACKET TOKEN ================================================
case class RightRoundBracketToken(
  lexeme: String = raw""")""",
  symbol: Symbol = 'RRBRACK
) extends Token
  with BracketTokenLike

object RightRoundBracketToken {
  val expected: String = raw""")"""
  val regex: Regex = raw"""^(\))(.*)""".r
}

//== LEFT SQUARE BRACKET TOKEN ================================================
case class LeftSquareBracketToken(
  lexeme: String = raw"""[""",
  symbol: Symbol = 'LSBRACK
) extends Token
  with BracketTokenLike

object LeftSquareBracketToken {
  val expected: String = raw"""["""
  val regex: Regex = raw"""^(\[)(.*)""".r
}

//== RIGHT SQUARE BRACKET TOKEN ===============================================
case class RightSquareBracketToken(
  lexeme: String = raw"""]""",
  symbol: Symbol = 'RSBRACK
) extends Token
  with BracketTokenLike

object RightSquareBracketToken {
  val expected: String = raw"""]"""
  val regex: Regex = raw"""^(\])(.*)""".r
}

//== LEFT CURLY BRACKET TOKEN =================================================
case class LeftCurlyBracketToken(
  lexeme: String = raw"""{""",
  symbol: Symbol = 'LCBRACK
) extends Token
  with BracketTokenLike

object LeftCurlyBracketToken {
  val expected: String = raw"""{"""
  val regex: Regex = raw"""^(\{)(.*)""".r
}

//== RIGHT CURLY BRACKET TOKEN ================================================
case class RightCurlyBracketToken(
  lexeme: String = raw"""}""",
  symbol: Symbol = 'RCBRACK
) extends Token
  with BracketTokenLike

object RightCurlyBracketToken {
  val expected: String = raw"""}"""
  val regex: Regex = raw"""^(\})(.*)""".r
}