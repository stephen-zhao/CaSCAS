//=============================================================================
// Interpreter.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

import scala.annotation._
import scala.io.StdIn
import scala.Console.{RESET, BOLD, UNDERLINED, println, print}
import org.cascas_project.cascas.parser.{Parser, ParseNode}

//=============================================================================
// TopLevelInterpretable trait
//
// Objects with this trait can be interpreted by the top level interpreter
//
trait TopLevelInterpretable {
  type TopLevelInterpretation = Symbol
  def interpretAsKind: TopLevelInterpretation
}

//=============================================================================
// Interpreter class
//
// Represents the interpreter, which works through a REP loop statement-by-
// -statement.
//
class Interpreter {

  var lexer: Lexer = new Lexer
  var parser: Parser = new Parser
  
  var globalScope: Scope = new Scope

  def repl(): Unit = {
    replrec()
  }

  def interpret(parseTree: TopLevelInterpretable): String = parseTree match {
    case _ if (parseTree.interpretAsKind == 'AssignmentInterpretation) => {
      interpretAsAssignment(parseTree)
    }
    case _ if (parseTree.interpretAsKind == 'ExpressionInterpretation) => {
      interpretAsExpression(parseTree)
    }
  }

  def interpretAsAssignment(parseTree: TopLevelInterpretable): String = {
    ""
    //TODO: do assignment to Scope, then return human-readable form indicating
    //      assignment was successful
    //
  }

  def interpretAsExpression(parseTree: TopLevelInterpretable): String = {
    ""
    //TODO: evaluate the expression, then return human-readable form of the
    //      evaluated expression
    //
  }

  @tailrec
  private def replrec(
    lineNum: Int = 0
  ): Unit = {

    Logger.info('REPL, "Waiting for input.")

    val prompt = f"${RESET}${BOLD}${UNDERLINED}In[$lineNum]:${RESET} "
    val promptCont = f"${RESET}${BOLD}..${RESET} "

    this.lexer.scanExpression(prompt, promptCont) match {
      case Vector() => Logger.info('REPL, "Empty input.")
      case exprAsTokens => {

        Logger.info('REPL, "Input string interpretted as tokens.")
        Logger.verbose('REPL, "Tokens are: \n" + exprAsTokens)

        val exprAsParseTree = this.parser.parse(parser.withoutUnparseables(exprAsTokens))
        
        Logger.info('REPL, "Input tokens parsed as tree")
        Logger.verbose('REPL, "Tree is:\n" + exprAsParseTree)

        val resultAsString = this.interpret(exprAsParseTree)
        
        // TODO: Do stuff with the tokens
        // for now, the parse tree is just get printed to the screen
        println(f"${RESET}${BOLD}${UNDERLINED}Out[$lineNum]:${RESET} $resultAsString")
        
        Logger.info('REPL, "Continuuing to next line of input.")

        // continue to the next iteration of the loop
        this.replrec(lineNum + 1)
      }
    }
  }

}
