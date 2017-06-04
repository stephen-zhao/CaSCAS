package org.cascas_project.cascas

import org.cascas_project.cascas._
import org.cascas_project.cascas.parser.Parser

/**
 * @author ${user.name}
 */
object App {
  
  def main(args : Array[String]) {
    val lexer: Lexer = new Lexer
    val parser: Parser = new Parser
    val toks = parser.withoutEsophagi(lexer.scanUntilEOF())
    println(parser.parse(toks))
  }

}
