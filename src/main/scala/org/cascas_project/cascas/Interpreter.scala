//=============================================================================
// Interpreter.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

import scala.annotation._
import scala.io.StdIn
import scala.Console.{RESET, BOLD, UNDERLINED, println, print}
import org.cascas_project.cascas.parser.{Parser, ParseNode}

//=============================================================================
// Interpretable trait
//
// Objects with this trait can be interpreted by the interpreter
//
trait Interpretable {
}

//=============================================================================
// Interpreter class
//
// Represents the interpreter, which works through a REP loop statement-by-
// -statement.
//
class Interpreter {

  var lexer: Lexer = new Lexer
  var parser: Parser = new Parser(this.lexer)
  
  var globalScope: Scope = new Scope

  def repl(): Unit = {
    replrec()
  }

  def interpret(parseTree: Interpretable): String = {
    ""
  }

  @tailrec
  private def replrec(
    lineNum: Int = 0
  ): Unit = {

    Logger.info('REPL, "Waiting for input.")

    val prompt = f"${RESET}${BOLD}${UNDERLINED}In[$lineNum]:${RESET} "
    val promptCont = f"${RESET}${BOLD}..${RESET} "

    this.parser.parse match {
      case None => {
        
        Logger.info('REPL, "Bad input.")

      }
      case Some(tree) => {

        Logger.info('REPL, "Input tokens parsed as tree")
        Logger.verbose('REPL, "Tree is:\n" + tree)

        val resultAsString = this.interpret(tree)
        
        // TODO: Do stuff with the tokens
        // for now, the parse tree is just get printed to the screen
        println(f"${RESET}${BOLD}${UNDERLINED}Out[$lineNum]:${RESET} $resultAsString")
      }
    }

    Logger.info('REPL, "Continuuing to next line of input.")

    // continue to the next iteration of the loop
    this.replrec(lineNum + 1)

  }

}
