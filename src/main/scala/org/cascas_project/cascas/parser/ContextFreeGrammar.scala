//=============================================================================
// parser/CFG.scala : CaSCAS Project
//=============================================================================
// This file contains the necessary modules used to describe a generic context-
// free grammar and to instantiate specifically a CaSCAS context-free grammar.

package org.cascas_project.cascas.parser

//=============================================================================

// This class represents a context free grammar definition
case class ContextFreeGrammar(
  nonterminals: Set[Symbol],
  terminals:    Set[Symbol],
  start:        Symbol,
  rules:        CFGRuleSet
) {
  // Throw runtime error if the lefthand-side of any rule is not a
  // nonterminal symbol
  require(this.rules forall (this.nonterminals contains _._1))

  // Union containing all symbols
  val allSymbols:   Set[Symbol] = this.nonterminals ++ this.terminals
  
  // Throw runtime error if the righthand-side of any rule contains
  // symbols not defined as either a nonterminal or a terminal
  require(this.rules forall (rule => rule._2 forall (rhs => this.allSymbols contains rhs)))

  // Throw compile-time error if the starting symbol is not in the set of
  // nonterminals
  require(this.nonterminals contains this.start)

  // Returns a set of all the rules originating from the start symbol
  def getStartingRules: CFGRuleSet = {
    this.rules.collect {
      case rule @ (lhs, rhs) if lhs == this.start => rule
    }
  }
}


