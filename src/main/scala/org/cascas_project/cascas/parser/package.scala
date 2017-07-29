package org.cascas_project.cascas

// This package contains modules used in parsing.

package object parser {

  private[parser] type CFGRule = (Symbol, Vector[Symbol])
  private[parser] type CFGRuleSet = Set[CFGRule]

  val CFG = ContextFreeGrammar(
    CaSCASCFGElements.nonterminals,
    CaSCASCFGElements.terminals,
    CaSCASCFGElements.start,
    CaSCASCFGElements.rules
  )

}
