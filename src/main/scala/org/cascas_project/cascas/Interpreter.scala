//=============================================================================
// Interpreter.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

import scala.annotation._
import scala.io.StdIn
import scala.Console.{RESET, BOLD, UNDERLINED, println}
import org.cascas_project.cascas.parser.Parser

class Interpreter {

  var lexer: Lexer = new Lexer
  var parser: Parser = new Parser

  def repl(): Unit = {
    replrec()
  }

  @tailrec
  private def replrec(
    lineNum: Int = 0
  ): Unit = {

    Logger.info('REPL, "Waiting for input.")

    print(f"${RESET}${BOLD}${UNDERLINED}In[$lineNum]:${RESET} ")

    lexer.scanLine() match {
      case Vector() => Logger.info('REPL, "Empty input.")
      case lineAsTokens => {

        Logger.info('REPL, "Input string interpretted as tokens.")
        Logger.verbose('REPL, "Tokens are: \n" + lineAsTokens)

        val lineAsParseTree = parser.parse(parser.withoutEsophagi(lineAsTokens))
        
        Logger.info('REPL, "Input tokens parsed as tree")
        Logger.verbose('REPL, "Tree is:\n" + lineAsParseTree)

        // TODO: Do stuff with the tokens
        // for now, the parse tree is just get printed to the screen
        println(f"${RESET}${BOLD}${UNDERLINED}Out[$lineNum]:${RESET} $lineAsParseTree")
        
        Logger.info('REPL, "Continuuing to next line of input.")

        // continue to the next iteration of the loop
        replrec(lineNum + 1)
      }
    }
  }

}
