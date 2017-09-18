//=============================================================================
// token/MiscTokens.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.token

//=============================================================================

import scala.util.matching.Regex

//=============================================================================

//== WHITESPACE TOKEN =========================================================
case class WhitespaceToken(
  lexeme: String,
  symbol: Symbol = 'WHITESPACE
) extends Token
  with WhitespaceTokenLike

object WhitespaceToken {
  val regex: Regex = raw"""^(\s+)(.*)""".r
}

//== COMMENT TOKEN ============================================================
case class CommentToken(
  lexeme: String,
  symbol: Symbol = 'COMMENT
) extends Token

object CommentToken {
  val regex: Regex = raw"""^(#.*)$$(.*)""".r
}

//== END OF LINE TOKEN ========================================================
//case class EndOfLineToken(
//  lexeme: String = "\n",
//  symbol: Symbol = 'EOL
//) extends Token
//
//object EndOfLineToken {
//}