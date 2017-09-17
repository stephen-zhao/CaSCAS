//=============================================================================
// token/NumberTokens.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.token

//=============================================================================

import scala.util.matching.Regex

//=============================================================================

//== INTEGER TOKEN ============================================================
case class IntegerToken(
  val lexeme: String,
  val symbol: Symbol = 'INT
) extends Token
  with NumberTokenLike

object IntegerToken {
  val regex: Regex = raw"""^((?:[0-9]_?)*[0-9])(.*)""".r
}

//== DECIMAL TOKEN ============================================================
case class DecimalToken(
  val lexeme: String,
  val symbol: Symbol = 'FLOAT
) extends Token
  with NumberTokenLike

object DecimalToken {
  val regex: Regex = raw"""^((?:[0-9]_?)*[0-9]?\.[0-9](?:_?[0-9])*)(.*)""".r
}