//=============================================================================
// Interpreter.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

import scala.annotation._
import scala.Console.{BOLD, RESET, UNDERLINED, println, print}
import scala.io.StdIn
import org.cascas_project.cascas.parser.{InteractiveParser, Parser, ParseNode}

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

  private val promptStyle = f"${BOLD}${UNDERLINED}"

  private var parser: Parser = new InteractiveParser(this)

  private var inputNum: Int = 0

  private var globalScope: Scope = new Scope

  def repl(): Unit = {
    replrec()
  }

  private def interpret(parseTree: Interpretable): String = {
    ""
  }

  @tailrec
  private def replrec(resetState: Boolean = true): Unit = {

    if (resetState) {
      this.inputNum = 0
      //TODO: reset the internal state
    }

    Logger.info('REPL, "Waiting for input.")

    this.parser.parseOption match {
      case None => {
        
        Logger.info('REPL, "Bad input.")

      }
      case Some(tree: Interpretable) => {

        Logger.info('REPL, "Input tokens parsed as tree")
        Logger.verbose('REPL, "Tree is:\n" + tree)

        val resultAsString = this.interpret(tree)
        
        // TODO: Do stuff with the tokens
        // for now, the parse tree is just get printed to the screen
        println(f"${RESET}" + this.promptStyle + f"Out[${this.inputNum}]:${RESET} $resultAsString")
      }

    }

    Logger.info('REPL, "Continuuing to next line of input.")

    this.inputNum += 1

    // continue to the next iteration of the loop
    this.replrec(false)

  }

  def displayInputPrompt(): Unit =
    print(f"${RESET}" + this.promptStyle + f"In[${this.inputNum}]:${RESET} ")

  def displayContinuedInputPrompt(): Unit =
    print(f"${RESET}" + this.promptStyle + f"..${RESET} ")

}
