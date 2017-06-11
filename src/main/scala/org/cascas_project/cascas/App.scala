//=============================================================================
// App.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

import org.cascas_project.cascas.parser.Parser

//=============================================================================
// Main Application Object
// 
object App {
  
  //===========================================================================
  // Main function
  //
  def main(args : Array[String]): Unit = {

    Logger.info('APP, f"""running \"CaSCAS ${("" /: args)(_ concat _)}\"""")
    
    // Catch program level exceptions
    // Bad practice to use try blocks in Scala
    // This is temporary, but may be here to stay
    try {
      Logger.info('APP, "CaSCAS is starting up.")

      val lexer: Lexer = new Lexer
      val parser: Parser = new Parser

      // Generate tokens and filter away unwanted ones
      val toks = parser.withoutEsophagi(lexer.scanUntilEOF())

      // Parse the tokens and print them to the screen
      println(parser.parse(toks))

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

}
