//=============================================================================
// token/WordTokens.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.token

//=============================================================================

import scala.util.matching.Regex

//=============================================================================

// == WORD TOKEN ==============================================================
case class WordToken(
  lexeme: String,
  symbol: Symbol = 'WORD
) extends Token

object WordToken {
  val regex: Regex = raw"""^([A-Za-z_\u03bb](?:\w)*)(.*)""".r
}

// == KEYWORD LAMBDA TOKEN ====================================================
case class KeywordLambdaToken(
  lexeme: String,
  symbol: Symbol = 'LAMBDA
) extends Token
  with KeywordTokenLike

object KeywordLambdaToken {
  val regex: Regex = (
    raw"""^(lambda|""" +
      '\u03bb'.toString +
      raw""")(.*)"""
    ).r
}

// == KEYWORD LET TOKEN =======================================================
case class KeywordLetToken(
  lexeme: String = raw"""let""",
  symbol: Symbol = 'LET
) extends Token
  with KeywordTokenLike

object KeywordLetToken {
  val expected: String = raw"""let"""
}

// == KEYWORD IF TOKEN ========================================================
case class KeywordIfToken(
  lexeme: String = raw"""if""",
  symbol: Symbol = 'IF
) extends Token
  with KeywordTokenLike

object KeywordIfToken {
  val expected: String = raw"""if"""
}

// == KEYWORD ELSIF TOKEN =====================================================
case class KeywordElsifToken(
  lexeme: String = raw"""elsif""",
  symbol: Symbol = 'ELSIF
) extends Token
  with KeywordTokenLike

object KeywordElsifToken {
  val expected: String = raw"""elsif"""
}

// == KEYWORD ELSE TOKEN ======================================================
case class KeywordElseToken(
  lexeme: String = raw"""else""",
  symbol: Symbol = 'ELSE
) extends Token
  with KeywordTokenLike

object KeywordElseToken {
  val expected: String = raw"""else"""
}

// == KEYWORD WHILE TOKEN =====================================================
case class KeywordWhileToken(
  lexeme: String = raw"""while""",
  symbol: Symbol = 'WHILE
) extends Token
  with KeywordTokenLike

object KeywordWhileToken {
  val expected: String = raw"""while"""
}

// == KEYWORD FOR TOKEN =======================================================
case class KeywordForToken(
  lexeme: String = raw"""for""",
  symbol: Symbol = 'FOR
) extends Token
  with KeywordTokenLike

object KeywordForToken {
  val expected: String = raw"""for"""
}

// == KEYWORD IN TOKEN ========================================================
case class KeywordInToken(
  lexeme: String = raw"""in""",
  symbol: Symbol = 'IN
) extends Token
  with KeywordTokenLike

object KeywordInToken {
  val expected: String = raw"""in"""
}
