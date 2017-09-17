//=============================================================================
// token/MiscTokens.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.token

//=============================================================================

import scala.util.matching.Regex

//=============================================================================

//== WHITESPACE TOKEN =========================================================
case class WhitespaceToken(
  val lexeme: String,
  val symbol: Symbol = 'WHITESPACE
) extends Token
  with WhitespaceTokenLike

object WhitespaceToken {
  val regex: Regex = raw"""^(\s+)(.*)""".r
}

//== COMMENT TOKEN ============================================================
case class CommentToken(
  val lexeme: String,
  val symbol: Symbol = 'COMMENT
) extends Token

object CommentToken {
  val regex: Regex = raw"""^(#.*)$$(.*)""".r
}

//== END OF LINE TOKEN ========================================================
//case class EndOfLineToken(
//  val lexeme: String = "\n",
//  val symbol: Symbol = 'EOL
//) extends Token
//
//object EndOfLineToken {
//}