package org.cascas_project.cascas

import org.cascas_project.cascas.tokens._

class Parser {
  def withoutEsophagi(tokens: Vector[Token]): Vector[Token] = {
    tokens.filter(t => t match { 
      case WhitespaceToken(_)   => false
      case CommentToken(_)      => false
      case _                    => true
    })
  }
}

object CaSCASCFG {
  //val CFG: PartialFunction[Symbol, 
}
