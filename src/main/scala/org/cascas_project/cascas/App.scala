package org.cascas_project.cascas

import org.cascas_project.cascas._

/**
 * @author ${user.name}
 */
object App {
  
  def main(args : Array[String]) {
    val lexer: Lexer = new Lexer
    println(lexer.scanUntilEOF)
  }

}