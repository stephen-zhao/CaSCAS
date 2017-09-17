//=============================================================================
// token/WordTokens.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.token

//=============================================================================

import scala.util.matching.Regex

//=============================================================================

// == WORD TOKEN ==============================================================
case class WordToken(
  val lexeme: String,
  val symbol: Symbol = 'WORD
) extends Token

object WordToken {
  val regex: Regex = raw"""^([A-Za-z_\u03bb](?:\w)*)(.*)""".r
}

// == KEYWORD LAMBDA TOKEN ====================================================
case class KeywordLambdaToken(
  val lexeme: String,
  val symbol: Symbol = 'LAMBDA
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
  val lexeme: String = raw"""let""",
  val symbol: Symbol = 'LET
) extends Token
  with KeywordTokenLike

object KeywordLetToken {
  val expected: String = raw"""let"""
}

// == KEYWORD IF TOKEN ========================================================
case class KeywordIfToken(
  val lexeme: String = raw"""if""",
  val symbol: Symbol = 'IF
) extends Token
  with KeywordTokenLike

object KeywordIfToken {
  val expected: String = raw"""if"""
}

// == KEYWORD ELSIF TOKEN =====================================================
case class KeywordElsifToken(
  val lexeme: String = raw"""elsif""",
  val symbol: Symbol = 'ELSIF
) extends Token
  with KeywordTokenLike

object KeywordElsifToken {
  val expected: String = raw"""elsif"""
}

// == KEYWORD ELSE TOKEN ======================================================
case class KeywordElseToken(
  val lexeme: String = raw"""else""",
  val symbol: Symbol = 'ELSE
) extends Token
  with KeywordTokenLike

object KeywordElseToken {
  val expected: String = raw"""else"""
}

// == KEYWORD WHILE TOKEN =====================================================
case class KeywordWhileToken(
  val lexeme: String = raw"""while""",
  val symbol: Symbol = 'WHILE
) extends Token
  with KeywordTokenLike

object KeywordWhileToken {
  val expected: String = raw"""while"""
}

// == KEYWORD FOR TOKEN =======================================================
case class KeywordForToken(
  val lexeme: String = raw"""for""",
  val symbol: Symbol = 'FOR
) extends Token
  with KeywordTokenLike

object KeywordForToken {
  val expected: String = raw"""for"""
}

// == KEYWORD IN TOKEN ========================================================
case class KeywordInToken(
  val lexeme: String = raw"""in""",
  val symbol: Symbol = 'IN
) extends Token
  with KeywordTokenLike

object KeywordInToken {
  val expected: String = raw"""in"""
}
