//=============================================================================
// Parser.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import scala.annotation._
import scala.Console.{RESET, println, print}
import org.cascas_project.cascas.Lexer
import org.cascas_project.cascas.Logger
import org.cascas_project.cascas.tokens._
import org.cascas_project.cascas.parser._


object Parser {

  // Parser Parse Status Enum
  sealed trait ParseStatus { def name: String }
  case object Success extends ParseStatus { val name = "Success" }
  case object Failure extends ParseStatus { val name = "Failure" }

}


class Parser {

  private val lrm: LRMachine = new LRMachineGenerator().generate

  private val lexer: Lexer = new Lexer

  private var numParseCalls: Int = 0

  protected val isIgnoredTokenDefault: Boolean = false

  protected val isIgnoredTokenFn: PartialFunction[Token, Boolean] = {
    case WhitespaceToken(_, _) => true
    case CommentToken(_, _)    => true
  }

  protected def displayPrompt(lineNum: Int): Unit = {}

  protected def displayContinuedPrompt(lineNum: Int): Unit = {}

  protected def scanTokens(
    lineNum: Int,
    isContinuedScan: Boolean
  ): Vector[Token] = this.lexer.scanProgram(
    displayPrompt=this.displayPrompt(lineNum),
    displayContPrompt=this.displayContinuedPrompt(lineNum),
    useContPrompt=isContinuedScan
  )

  def parse(): Option[ParseNode] = {
    val result = this.parseUntilValidProgram()
    this.numParseCalls = this.numParseCalls + 1
    result
  }

  @tailrec
  private def parseUntilValidProgram(
    isFirst: Boolean = true
  ): Option[ParseNode] = {

    val tokens = this.scanTokens(this.numParseCalls,!isFirst).
                 filter(this.isNotIgnoredToken)

    if (tokens.isEmpty) {
      
      Logger.info('PARSER, "No tokens parsed.")
      
      None

    }
    else {

      this.lrm.rightmostDerive(tokens, isFirst) match {
        case None => {
          this.lrm.getDerivationStatus match {
            case LRMachine.Failure => {
              
              Logger.error('PARSER, "Tokens failed to parse.")
              None

            }
            case LRMachine.NeedsTokens => {

              Logger.info('PARSER, "Tokens are parsing. Need more to continue...")
              this.parseUntilValidProgram(false)

            }
            case others => {
              val err = "Invalid LRMachine status for no parse tree returned." +
                        f" Status=${others.name}"
              Logger.error('PARSER, err)
              throw new Exception(err)
            }
          }
        }
        case Some(tree) => {

          Logger.info('PARSER, "Tokens successfully parsed.")
          Some(tree)

        }
      }

    }
  }

  private def isNotIgnoredToken(token: Token): Boolean = {
    !( if (this.isIgnoredTokenFn.isDefinedAt(token)) {
        this.isIgnoredTokenFn(token)
      }
      else {
        this.isIgnoredTokenDefault
      }
    )
  }

}


class InteractiveParser(
  val promptStyle: String
) extends Parser {

  protected override def displayPrompt(lineNum: Int): Unit = {
    print(f"${RESET}" + this.promptStyle + f"In[$lineNum]:${RESET} ")
  }

  protected override def displayContinuedPrompt(lineNum: Int): Unit = {
    print(f"${RESET}" + this.promptStyle + f"..${RESET} ")
  }

}
