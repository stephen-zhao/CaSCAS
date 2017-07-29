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

  private val errorTokenRegex: Regex =
    raw"""^([^/s]+)(.*)""".r

  private def takeToken(str: String): (Token, String) = str match {
    // Whitespace tokens
    case WhitespaceToken.regex(s, r)           => (WhitespaceToken(s), r)

    // NumberTokenLike tokens
    case DecimalToken.regex(s, r)              => (DecimalToken(s), r)
    case IntegerToken.regex(s, r)              => (IntegerToken(s), r)

    // BoolOperatorTokenLike tokens
    case BoolOperatorAndToken.regex(s, r)      => (BoolOperatorAndToken(), r)
    case BoolOperatorOrToken.regex(s, r)       => (BoolOperatorOrToken(), r)
    case BoolOperatorNotToken.regex(s, r)      => (BoolOperatorNotToken(), r)

    // OperatorTokenLike tokens
    case OperatorPlusToken.regex(s, r)         => (OperatorPlusToken(), r)
    case OperatorMinusToken.regex(s, r)        => (OperatorMinusToken(), r)
    case OperatorMultToken.regex(s, r)         => (OperatorMultToken(), r)
    case OperatorDivToken.regex(s, r)          => (OperatorDivToken(), r)
    case OperatorPowToken.regex(s, r)          => (OperatorPowToken(), r)
    case OperatorBangToken.regex(s, r)         => (OperatorBangToken(), r)

    // BracketTokenLike tokens
    case LeftRoundBracketToken.regex(s, r)     => {
      this.bracketStack.push('RBracket)
      (LeftRoundBracketToken(), r)
    }
    case RightRoundBracketToken.regex(s, r)    => {
      this.bracketStack.pop('RBracket)
      (RightRoundBracketToken(), r)
    }
    case LeftSquareBracketToken.regex(s, r)    => {
      this.bracketStack.push('SBracket)
      (LeftSquareBracketToken(), r)
    }
    case RightSquareBracketToken.regex(s, r)   => {
      this.bracketStack.pop('SBracket)
      (RightSquareBracketToken(), r)
    }
    case LeftCurlyBracketToken.regex(s, r)     => {
      this.bracketStack.push('CBracket)
      (LeftCurlyBracketToken(), r)
    }
    case RightCurlyBracketToken.regex(s, r)    => {
      this.bracketStack.pop('CBracket)
      (RightCurlyBracketToken(), r)
    }

    // RelationTokenLike tokens
    case RelationLessEqualToken.regex(s, r)    => (RelationLessEqualToken(), r)
    case RelationGreaterEqualToken.regex(s, r) => (RelationGreaterEqualToken(), r)
    case RelationEqualToken.regex(s, r)        => (RelationEqualToken(), r)
    case RelationNotEqualToken.regex(s, r)     => (RelationNotEqualToken(), r)
    case RelationGreaterToken.regex(s, r)      => (RelationGreaterToken(), r)
    case RelationLessToken.regex(s, r)         => (RelationLessToken(), r)

    // Punctuation tokens
    case SemicolonToken.regex(s, r)            => (SemicolonToken(s), r)
    case CommaToken.regex(s, r)                => (CommaToken(), r)

    // Comments
    case CommentToken.regex(s, r)              => (CommentToken(s), r)

    // Assignment operator token
    case AssignmentToken.regex(s, r)           => (AssignmentToken(), r)

    // WordTokens, i.e. identifiers, function names, symbols, variables, etc
    case WordToken.regex(s, r)                 => s match {
      case KeywordLambdaToken.regex(ss, _) => (KeywordLambdaToken(ss), r)
      case KeywordLetToken.expected   => (KeywordLetToken(), r)
      case KeywordIfToken.expected    => (KeywordIfToken(), r)
      case KeywordElsifToken.expected => (KeywordElsifToken(), r)
      case KeywordElseToken.expected  => (KeywordElseToken(), r)
      case KeywordWhileToken.expected => (KeywordWhileToken(), r)
      case KeywordForToken.expected   => (KeywordForToken(), r)
      case KeywordInToken.expected    => (KeywordInToken(), r)
      case _                          => (WordToken(s), r)
    }

    // Error up-to-whitespace case
    case line @ this.errorTokenRegex(s, r) => {
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
      this.takeToken(line) match {
        case (token, rest) =>
          this.getAllTokens(rest, accuTokens :+ token)
      }
    }
  }
  
  protected def inputRead(): String = {
    StdIn.readLine()
  }

  private def scan(getInput: => String = inputRead): Option[Vector[Token]] = {
    getInput match {
      case null => None
      case line => Some(this.getAllTokens(line))
    }
  }

  def scanLine(getInput: => String = inputRead): Vector[Token] = {
    this.scan(getInput).getOrElse(Vector[Token]())
  }

  @tailrec
  private def scanProgramRec(
    displayPrompt: => Unit    = Unit,
    getInput: => String       = inputRead,
    accuTokens: Vector[Token] = Vector[Token]()
  ): Vector[Token] = {
    val tokens = accuTokens ++ this.scan(getInput).getOrElse(Vector[Token]())
    if (this.bracketStack.isEmpty) {
      tokens
    }
    else {
      displayPrompt
      this.scanProgramRec(displayPrompt, getInput, tokens)
    }
  }

  def scanProgram(
    displayPrompt: => Unit     = Unit,
    displayContPrompt: => Unit = Unit,
    getInput: => String        = inputRead,
    useContPrompt: Boolean     = false
  ): Vector[Token] = {
    if (useContPrompt) displayContPrompt else displayPrompt
    this.scanProgramRec(displayContPrompt)
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
