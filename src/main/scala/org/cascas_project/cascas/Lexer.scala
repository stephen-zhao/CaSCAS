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
        case WhitespaceToken.regex(s, _*)     => WhitespaceToken(s)
        case IntegerToken.regex(s, _*)        => IntegerToken(s)
        case DecimalToken.regex(s, _*)        => DecimalToken(s)
        case OperatorPlusToken.regex(s)       => OperatorPlusToken()
        case OperatorMinusToken.regex(s)      => OperatorMinusToken()
        case OperatorMultToken.regex(s)       => OperatorMultToken()
        case OperatorDivToken.regex(s)        => OperatorDivToken()
        case OperatorPowToken.regex(s)        => OperatorPowToken()
        case OperatorBangToken.regex(s)       => OperatorBangToken()
        case LeftRoundBracketToken.regex(s)   => LeftRoundBracketToken()
        case RightRoundBracketToken.regex(s)  => RightRoundBracketToken()
        case LeftSquareBracketToken.regex(s)  => LeftSquareBracketToken()
        case RightSquareBracketToken.regex(s) => RightSquareBracketToken()
        case LeftCurlyBracketToken.regex(s)   => LeftCurlyBracketToken()
        case RightCurlyBracketToken.regex(s)  => RightCurlyBracketToken()
        /*
        case RoundBracketToken.regex(s)   => {
          s match {
            case RoundBracketToken.expectedLexeme(BracketLeft)    => RoundBracketToken(s, BracketLeft)
            case RoundBracketToken.expectedLexeme(BracketRight)   => RoundBracketToken(s, BracketRight)
          }
        }
        case SquareBracketToken.regex(s) => {
          s match {
            case SquareBracketToken.expectedLexeme(BracketLeft)   => SquareBracketToken(s, BracketLeft)
            case SquareBracketToken.expectedLexeme(BracketRight)  => SquareBracketToken(s, BracketRight)
          }
        }
        case CurlyBracketToken.regex(s)  => {
          s match {
            case CurlyBracketToken.expectedLexeme(BracketLeft)    => CurlyBracketToken(s, BracketLeft)
            case CurlyBracketToken.expectedLexeme(BracketRight)   => CurlyBracketToken(s, BracketRight)
          }
        }
        */
        case OtherToken.regex(s, _*)          => OtherToken(s)
        case _                                => sys.error(f"ERROR: Cannot tokenize $line")
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
