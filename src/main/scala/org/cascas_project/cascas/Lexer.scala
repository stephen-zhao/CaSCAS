//=============================================================================
// Lexer.scala : CaSCAS Project
//=============================================================================

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
        case DecimalToken.regex(s, _*)          => DecimalToken(s)
        case IntegerToken.regex(s, _*)          => IntegerToken(s)
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
        // Comma tokens
        case CommaToken.regex(s)                => CommaToken()
        // Comments
        case CommentToken.regex(s)              => CommentToken(s)
        // Assignment operator token
        case AssignmentToken.regex(s)           => AssignmentToken()
        // WordTokens, i.e. identifiers, function names, symbols, variables, etc
        case WordToken.regex(s, _*)             => s match {
          //case KeywordDefToken.expected => KeywordDefToken()
          case _                        => WordToken(s)
        }
        // Error case
        case _                                  => sys.error(f"ERROR: Cannot tokenize $line")
      }
      token +: getAllTokens(line.drop(token.length))
    }
  }

  def scan(getInput: () => String = () => inputRead): Option[Vector[Token]] = {
    getInput() match {
      case null => None
      case line => Some(getAllTokens(line))
    }
  }

  def scanLine(getInput: () => String = () => inputRead): Vector[Token] = {
    scan(getInput).getOrElse(Vector[Token]())
  }

  def scanUntilEOF(getInput: () => String = () => inputRead): Vector[Token] = {
    scan(getInput) match {
      case None                         => Vector[Token]()
      case Some(tokens: Vector[Token])  => tokens ++ scanUntilEOF()
    }
  }
}
