//=============================================================================
// Parser.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import scala.annotation._
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


class Parser(
  val lexer: Lexer
) {

  val lrm: LRMachine = new LRMachineGenerator().generate

  var numParseCalls: Int = 0

  def parse(): Option[ParseNode] = {
    this.numParseCalls += 1
    this.parseUntilValidProgram()
  }

  @tailrec
  private def parseUntilValidProgram(
    isFirst: Boolean = true
  ): Option[ParseNode] = {

    val tokens = this.withoutUnparseables(this.lexer.scanProgram())

    this.lrm.rightmostDerive(tokens, isFirst) match {
      case None => {
        this.lrm.getDerivationStatus match {
          case LRMachine.Failure => {
            None
          }
          case LRMachine.NeedsTokens => {
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
        Some(tree)
      }
    }
  }

  private def withoutUnparseables(tokens: Vector[Token]): Vector[Token] = {
    tokens.filter(t => t match { 
      case _: WhitespaceToken   => false
      case _: CommentToken      => false
      case _                    => true
    })
  }


}
