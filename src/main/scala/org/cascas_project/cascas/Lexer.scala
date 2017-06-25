//=============================================================================
// Lexer.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

import java.io._
import scala.annotation._
import scala.io.StdIn
import scala.util.matching._
import org.cascas_project.cascas.tokens._

//=============================================================================
// Lexer class
//
// Performs lexical analysis on inputted text to produce tokens recognized by
// the CaSCAS parser.
//
class Lexer {

  private var bracketStack: BracketStack = new BracketStack

  private val errorTokenRegex: UnanchoredRegex =
    raw"""^([^/s]+)""".r.unanchored

  private def getToken(str: String): Token = str match {
    // Whitespace tokens
    case NewlineToken.regex(s, _*)          => NewlineToken(s)
    case WhitespaceToken.regex(s, _*)       => WhitespaceToken(s)

    // NumberTokenLike tokens
    case DecimalToken.regex(s, _*)          => DecimalToken(s)
    case IntegerToken.regex(s, _*)          => IntegerToken(s)

    // BoolOperatorTokenLike tokens
    case BoolOperatorAndToken.regex(s)      => BoolOperatorAndToken()
    case BoolOperatorOrToken.regex(s)       => BoolOperatorOrToken()
    case BoolOperatorNotToken.regex(s)      => BoolOperatorNotToken()

    // OperatorTokenLike tokens
    case OperatorPlusToken.regex(s)         => OperatorPlusToken()
    case OperatorMinusToken.regex(s)        => OperatorMinusToken()
    case OperatorMultToken.regex(s)         => OperatorMultToken()
    case OperatorDivToken.regex(s)          => OperatorDivToken()
    case OperatorPowToken.regex(s)          => OperatorPowToken()
    case OperatorBangToken.regex(s)         => OperatorBangToken()

    // BracketTokenLike tokens
    case LeftRoundBracketToken.regex(s)     => {
      this.bracketStack.push('RBracket)
      LeftRoundBracketToken()
    }
    case RightRoundBracketToken.regex(s)    => {
      this.bracketStack.pop('RBracket)
      RightRoundBracketToken()
    }
    case LeftSquareBracketToken.regex(s)    => {
      this.bracketStack.push('SBracket)
      LeftSquareBracketToken()
    }
    case RightSquareBracketToken.regex(s)   => {
      this.bracketStack.pop('SBracket)
      RightSquareBracketToken()
    }
    case LeftCurlyBracketToken.regex(s)     => {
      this.bracketStack.push('CBracket)
      LeftCurlyBracketToken()
    }
    case RightCurlyBracketToken.regex(s)    => {
      this.bracketStack.pop('CBracket)
      RightCurlyBracketToken()
    }

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
    case WordToken.regex(s, _*)         => s match {
      case KeywordLetToken.expected   => KeywordLetToken()
      case KeywordIfToken.expected    => KeywordIfToken()
      case KeywordElsifToken.expected => KeywordElsifToken()
      case KeywordElseToken.expected  => KeywordElseToken()
      case KeywordWhileToken.expected => KeywordWhileToken()
      case KeywordForToken.expected   => KeywordForToken()
      case KeywordInToken.expected    => KeywordInToken()
      case _                          => WordToken(s)
    }

    // Error up-to-whitespace case
    case line @ this.errorTokenRegex(s, _*) => {
      val err = f"""Invalid token: \"$s\" at beginning of line \"$line\""""
      Logger.error('LEXER, err)
      throw new Exception(err)
    }

    // All other error cases
    case line => {
      val err = f"""Could not tokenize \"$line\""""
      Logger.error('LEXER, err)
      throw new Exception(err)
    }
  }

  @tailrec
  private def getAllTokens(
      line: String, 
      accuTokens: Vector[Token] = Vector[Token]()
    ): Vector[Token] = {
    if (line.isEmpty) {
      accuTokens
    }
    else {
      val token: Token = this.getToken(line)
      this.getAllTokens(line.drop(token.length), accuTokens :+ token)
    }
  }
  
  def inputRead(): String = {
    StdIn.readLine()
  }

  def scan(getInput: => String = inputRead): Option[Vector[Token]] = {
    getInput match {
      case null => None
      case line => Some(this.getAllTokens(line))
    }
  }

  def scanLine(getInput: => String = inputRead): Vector[Token] = {
    this.scan(getInput).getOrElse(Vector[Token]())
  }

  @tailrec
  private def scanExpressionRec(
    promptCont: => String     = ".. ",
    getInput: => String       = inputRead,
    accuTokens: Vector[Token] = Vector[Token]()
  ): Vector[Token] = {
    val tokens = accuTokens ++ this.scan(getInput).getOrElse(Vector[Token]())
    if (this.bracketStack.isEmpty) {
      tokens
    }
    else {
      print(promptCont)
      this.scanExpressionRec(promptCont, getInput, tokens)
    }
  }

  def scanExpression(
    prompt: => String          = "In: ",
    promptCont: => String      = ".. ",
    getInput: => String        = inputRead
  ): Vector[Token] = {
    print(prompt)
    this.scanExpressionRec(promptCont)
  }

  def scanUntilEOF(getInput: => String = inputRead): Vector[Token] = {
    this.scan(getInput) match {
      case None                         => Vector[Token]()
      case Some(tokens: Vector[Token])  => tokens ++ scanUntilEOF()
    }
  }
}


class BracketStack {

  private var theStack: List[Symbol] = List[Symbol]()

  private val acceptable: Set[Symbol] = 
    Set('RBracket, 'SBracket, 'CBracket)

  def push(bracket: Symbol): BracketStack = {
    require(this.acceptable contains bracket)
    this.theStack = bracket :: this.theStack
    this
  }

  def pop(bracket: Symbol): Unit = {
    require(this.acceptable contains bracket)
    if (bracket == this.theStack.head) {
      this.theStack = this.theStack.tail
    }
  }

  def popOption(bracket: Symbol): Option[Symbol] = {
    require(this.acceptable contains bracket)
    if (bracket == this.theStack.head) {
      this.theStack = this.theStack.tail
      Some(bracket)
    }
    else {
      None
    }
  }

  def isEmpty(): Boolean = {
    this.theStack.isEmpty
  }

}
