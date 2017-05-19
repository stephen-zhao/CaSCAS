package org.cascas_project.cascas

import java.io._
import scala.io.StdIn
import scala.util.matching.Regex
import org.cascas_project.cascas.tokens._

class Lexer {
  def inputRead(): String = {
    StdIn.readLine()
  }

  def getAllTokens(line: String): Vector[Token] = {
    if (line.isEmpty) {
      Vector[Token]()
    }
    else {
      val token: Token = line match {
        // Whitespace tokens
        case WhitespaceToken.regex(s, _*)       => WhitespaceToken(s)
        // NumberTokenLike tokens
        case IntegerToken.regex(s, _*)          => IntegerToken(s)
        case DecimalToken.regex(s, _*)          => DecimalToken(s)
        // OperatorTokenLike tokens
        case OperatorPlusToken.regex(s)         => OperatorPlusToken()
        case OperatorMinusToken.regex(s)        => OperatorMinusToken()
        case OperatorMultToken.regex(s)         => OperatorMultToken()
        case OperatorDivToken.regex(s)          => OperatorDivToken()
        case OperatorPowToken.regex(s)          => OperatorPowToken()
        case OperatorBangToken.regex(s)         => OperatorBangToken()
        // BracketTokenLike tokens
        case LeftRoundBracketToken.regex(s)     => LeftRoundBracketToken()
        case RightRoundBracketToken.regex(s)    => RightRoundBracketToken()
        case LeftSquareBracketToken.regex(s)    => LeftSquareBracketToken()
        case RightSquareBracketToken.regex(s)   => RightSquareBracketToken()
        case LeftCurlyBracketToken.regex(s)     => LeftCurlyBracketToken()
        case RightCurlyBracketToken.regex(s)    => RightCurlyBracketToken()
        // RelationTokenLike tokens
        case RelationLessEqualToken.regex(s)    => RelationLessEqualToken()
        case RelationGreaterEqualToken.regex(s) => RelationGreaterEqualToken()
        case RelationEqualToken.regex(s)        => RelationEqualToken()
        case RelationNotEqualToken.regex(s)     => RelationNotEqualToken()
        case RelationGreaterToken.regex(s)      => RelationGreaterToken()
        case RelationLessToken.regex(s)         => RelationLessToken()
        // WordTokens, i.e. identifiers, function names, symbols, variables, etc
        case WordToken.regex(s, _*)             => WordToken(s)
        // Error case
        case _                                  => sys.error(f"ERROR: Cannot tokenize $line")
      }
      token +: getAllTokens(line.drop(token.length))
    }
  }

  def scan(): Option[Vector[Token]] = {
    inputRead match {
      case null => None
      case line => Some(getAllTokens(line))
    }
  }

  def scanUntilEOF(): Vector[Token] = {
    scan match {
      case None                         => Vector[Token]()
      case Some(tokens: Vector[Token])  => tokens ++ scanUntilEOF
    }
  }
}
