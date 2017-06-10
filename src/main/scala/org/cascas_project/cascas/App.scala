//=============================================================================
// App.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

import org.cascas_project.cascas.parser.Parser

object App {
  
  def main(args : Array[String]) {
    
    try {
      Logger.info('APP, "CaSCAS is starting up.")

      val lexer: Lexer = new Lexer
      val parser: Parser = new Parser
      val toks = parser.withoutEsophagi(lexer.scanUntilEOF())
      println(parser.parse(toks))

      Logger.info('APP, "CaSCAS execution complete.")
    }
    catch {
      case e: Throwable => Logger.exception('APP, e)
    }
  }

}
