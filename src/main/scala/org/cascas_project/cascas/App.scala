//=============================================================================
// App.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

import scala.annotation._
import scala.io.StdIn
import org.cascas_project.cascas.parser.Parser

//=============================================================================
// Main Application Object
// 
object App {
  
  //===========================================================================
  // Main function
  //
  def main(args : Array[String]): Unit = {

    Logger.info('APP, f"""running "${("CaSCAS" /: args)((x,y) => " " + x + y)}"""")
    
    // Catch program level exceptions
    // Bad practice to use try blocks in Scala
    // This is temporary, but may be here to stay
    try {
      Logger.info('APP, "CaSCAS is starting up.")

      val lexer: Lexer = new Lexer
      val parser: Parser = new Parser
      
      repl()

      Logger.info('APP, "CaSCAS has terminated successfully.")
    }
    catch {
      // Catch all exceptions
      case e: Throwable => {
        Logger.exception('APP, e)
        Logger.error('APP, "CaSCAS has terminated unexpectedly.")
      }
    }
  }

  //===========================================================================
  // REPL
  //
  @tailrec
  def repl(
    lineNum: Int = 0,
    lexer: Lexer = new Lexer,
    parser: Parser = new Parser
  ): Unit = {
    print(f"[$lineNum]: ")
    lexer.scanLine() match {
      case Vector() => Logger.info('APP, "Empty input")
      case lineAsTokens => {

        val lineAsParseTree = parser.parse(parser.withoutEsophagi(lineAsTokens))
        // TODO: Do stuff with the tokens
        // for now, the parse tree is just get printed to the screen
        println(lineAsParseTree)
        
        // continue to the next iteration of the loop
        repl(lineNum + 1, lexer, parser)
      }
    }
  }

}
