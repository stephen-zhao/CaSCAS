//=============================================================================
// CFG.scala : CaSCAS Project
//=============================================================================
// This file contains the necessary modules used to describe a generic context-
// free grammar and to instantiate specifically a CaSCAS context-free grammar.

package org.cascas_project.cascas.parser

// This class represents a context free grammar definition
case class ContextFreeGrammar(
  val nonterminals: Set[Symbol],
  val terminals:    Set[Symbol],
  val start:        Symbol,
  val rules:        CFGRuleSet
) {
  // Throw compile-time error if the lefthand-side of any rule is not a
  // nonterminal symbol.
  require(this.rules forall (this.nonterminals contains _._1))

  // Union containing all symbols
  val allSymbols:   Set[Symbol] = this.nonterminals ++ this.terminals
  
  // Throw compile-time error if the righthand-side of any rule contains
  // symbols not defined as either a nonterminal or a terminal
  require(this.rules forall (_._2 forall (this.allSymbols contains _)))

  // Throw compile-time error if the starting symbol is not in the set of
  // nonterminals
  require(this.nonterminals contains this.start)

  // Returns a set of all the rules originating from the start symbol
  def getStartingRules(): CFGRuleSet = {
    this.rules.collect {
      case rule @ (lhs, rhs) if (lhs == this.start) => rule 
    }
  }
}

// Static object to hold the specific definitions for the elements used in
// CaSCAS's context-free grammar.
private[parser] object CaSCASCFGElements {

  val nonterminals: Set[Symbol] = 
    Set('Program, 'Statements, 'Statement, 'Assign, 'ReAssign, 'Expr, 'Control,
        'IfControl, 'ElControl, 'WhileControl, 'ForControl, 'Collection,
        'Set, 'SetIn, 'List, 'ListIn, 'BoolExpr, 'BoolOrend, 'BoolAndend,
        'Relation, 'MathExpr, 'Term, 'Factor, 'Exponent, 'Base, 'Lambda, 
        'Apply, 'FParams, 'AParams)
  
  val terminals: Set[Symbol] = 
    Set('LET, 'IF, 'ELSIF, 'ELSE, 'WHILE, 'FOR, 'IN,
        'WORD, 'INT, 'FLOAT, 'COMMA, 'ASSIGN, 'SEMICOLON,
        'LRBRACK, 'RRBRACK, 'LCBRACK, 'RCBRACK, 'LSBRACK, 'RSBRACK, 
        'OR, 'AND, 'NOT, 'EQ, 'NEQ, 'GT, 'LT, 'GTE, 'LTE,
        'PLUS, 'MINUS, 'STAR, 'SLASH, 'BANG, 'POW, 'LAMBDA)

  val start: Symbol = 'Program

  // A smaller set of rules to test LRMachine Generation
  //val rules: RuleSet = Set[Rule](
  //  ('Statement,  Vector('Assign)),
  //  ('Statement,  Vector('Expr)),
  //  ('Assign,     Vector('ID, 'ASSIGN, 'INT)),
  //  ('Expr,       Vector('Factor, 'PLUS, 'Factor)),
  //  ('Factor,     Vector('INT)),
  //  ('Factor,     Vector('WORD)),
  //  ('Factor,     Vector('LRBRACK, 'Statement, 'RRBRACK))
  //)

  val rules: CFGRuleSet = Set[CFGRule](
    ('Program,    Vector('Statements)),

    ('Statements, Vector('Statements, 'SEMICOLON, 'Statement)),
    ('Statements, Vector('Statement)),

    ('Statement,  Vector('Assign)),
    ('Statement,  Vector('ReAssign)),
    ('Statement,  Vector('Expr)),

    ('Assign,     Vector('LET, 'WORD, 'ASSIGN, 'Expr)),
    ('Assign,     Vector('LET, 'WORD, 'LRBRACK, 'FParams, 'RRBRACK, 'ASSIGN, 'Expr)),

    ('ReAssign,   Vector('WORD, 'ASSIGN, 'Expr)),

    ('Expr,       Vector('BoolExpr)),

    ('BoolExpr,   Vector('BoolExpr, 'OR, 'BoolOrend)),
    ('BoolExpr,   Vector('BoolOrend)),

    ('BoolOrend,  Vector('BoolOrend, 'AND, 'BoolAndend)),
    ('BoolOrend,  Vector('BoolAndend)),

    ('BoolAndend, Vector('NOT, 'BoolAndend)),
    ('BoolAndend, Vector('Relation)),

    ('Relation,   Vector('Relation, 'EQ, 'MathExpr)),
    ('Relation,   Vector('Relation, 'NEQ, 'MathExpr)),
    ('Relation,   Vector('Relation, 'GT, 'MathExpr)),
    ('Relation,   Vector('Relation, 'LT, 'MathExpr)),
    ('Relation,   Vector('Relation, 'GE, 'MathExpr)),
    ('Relation,   Vector('Relation, 'LE, 'MathExpr)),
    ('Relation,   Vector('MathExpr)),

    ('MathExpr,   Vector('MathExpr, 'PLUS, 'Term)),
    ('MathExpr,   Vector('MathExpr, 'MINUS, 'Term)),
    ('MathExpr,   Vector('Term)),

    ('Term,       Vector('Term, 'STAR, 'Factor)),
    ('Term,       Vector('Term, 'SLASH, 'Factor)),
    ('Term,       Vector('Factor)),

    ('Factor,     Vector('MINUS, 'Factor)),
    ('Factor,     Vector('Factor, 'BANG)),
    ('Factor,     Vector('Exponent)),

    ('Exponent,   Vector('Base, 'POW, 'Exponent)),
    ('Exponent,   Vector('Base)),
    
    ('Base,       Vector('WORD)),
    ('Base,       Vector('INT)),
    ('Base,       Vector('FLOAT)),
    ('Base,       Vector('Apply)),
    ('Base,       Vector('Lambda)),
    ('Base,       Vector('Collection)),
    ('Base,       Vector('Control)),
    ('Base,       Vector('LRBRACK, 'Statements, 'RRBRACK)),
    
    ('Lambda,     Vector('LAMBDA, 'LRBRACK, 'FParams, 'RRBRACK, 'LRBRACK,
                         'Statements, 'RRBRACK)),

    ('Apply,      Vector('Apply, 'LRBRACK, 'AParams, 'RRBRACK)),
    ('Apply,      Vector('Lambda, 'LRBRACK, 'AParams, 'RRBRACK)),
    ('Apply,      Vector('WORD, 'LRBRACK, 'AParams, 'RRBRACK)),

    ('FParams,    Vector('FParams, 'COMMA, 'WORD)),
    ('FParams,    Vector('WORD)),

    ('AParams,    Vector('AParams, 'COMMA, 'Expr)),
    ('AParams,    Vector('Expr)),
    
    ('Control,    Vector('IfControl)),
    ('Control,    Vector('WhileControl)),
    ('Control,    Vector('ForControl)),

    ('Collection, Vector('Set)),
    ('Collection, Vector('List)),
    
    ('IfControl,  Vector('IF, 'LRBRACK, 'Expr, 'RRBRACK, 'LRBRACK, 
                         'Statements, 'RRBRACK, 'ElControl)),
    ('ElControl,  Vector('ELSIF, 'LRBRACK, 'Expr, 'RRBRACK, 'LRBRACK, 
                         'Statements, 'RRBRACK, 'ElControl)),
    ('ElControl,  Vector('ELSE, 'LRBRACK, 'Statements, 'RRBRACK)),

    ('WhileControl, Vector('WHILE, 'LRBRACK, 'Expr, 'RRBRACK, 
                         'LRBRACK, 'Statements, 'RRBRACK)),

    ('ForControl, Vector('FOR, 'LRBRACK, 'WORD, 'IN, 'Collection, 
                         'RRBRACK, 'LRBRACK, 'Statements, 'RRBRACK)),

    ('Set,        Vector('LCBRACK, 'RCBRACK)),
    ('Set,        Vector('LCBRACK, 'SetIn, 'RCBRACK)),
    ('SetIn,      Vector('SetIn, 'COMMA, 'Expr)),
    ('SetIn,      Vector('Expr)),

    ('List,       Vector('LSBRACK, 'RSBRACK)),
    ('List,       Vector('LSBRACK, 'ListIn, 'RSBRACK)),
    ('ListIn,     Vector('ListIn, 'COMMA, 'Expr)),
    ('ListIn,     Vector('Expr))
  )

}
