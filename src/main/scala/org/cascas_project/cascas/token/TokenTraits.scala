//=============================================================================
// token/TokenTraits.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.token

//=============================================================================

abstract class Token
{
  def lexeme: String
  def symbol: Symbol
  def length: Int = {
    lexeme.length
  }
}

trait NumberTokenLike

trait OperatorTokenLike

trait BoolOperatorTokenLike

trait BracketTokenLike

trait RelationTokenLike

trait WhitespaceTokenLike

trait KeywordTokenLike