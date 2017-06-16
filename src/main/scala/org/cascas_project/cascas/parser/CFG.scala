//=============================================================================
// CFG.scala : CaSCAS Project
//=============================================================================
package org.cascas_project.cascas.parser

object CFG {

  type Rule = (Symbol, Vector[Symbol])
  type RuleSet = Set[Rule]
  
  val nonterminals: Set[Symbol] = 
    Set('Statement, 'Assign, 'Expr, 'Set, 'SetIn, 'List, 'ListIn, 'MathExpr, 'Term, 
        'Factor, 'FnCall, 'Num, 'FParams, 'AParams)
  
  val terminals: Set[Symbol] = 
    Set('WORD, 'INT, 'FLOAT, 'COMMA, 'LRBRACK, 'RRBRACK, 'LCBRACK, 'RCBRACK,
        'LSBRACK, 'RSBRACK, 'ASSIGN, 'EQ, 'NEQ, 'GT, 'LT, 'GTE, 'LTE, 'PLUS,
        'MINUS, 'STAR, 'SLASH, 'BANG, 'POW)

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

  val rules: RuleSet = Set[Rule](
    ('Statement,  Vector('Assign)),
    ('Statement,  Vector('Expr)),
    ('Assign,     Vector('ID, 'ASSIGN, 'Expr)),
    ('Assign,     Vector('ID, 'LRBRACK, 'FParams, 'RRBRACK, 'ASSIGN, 'MathExpr)),
    ('Expr,       Vector('MathExpr, 'EQ, 'MathExpr)),
    ('Expr,       Vector('MathExpr, 'NEQ, 'MathExpr)),
    ('Expr,       Vector('MathExpr, 'GT, 'MathExpr)),
    ('Expr,       Vector('MathExpr, 'LT, 'MathExpr)),
    ('Expr,       Vector('MathExpr, 'GE, 'MathExpr)),
    ('Expr,       Vector('MathExpr, 'LE, 'MathExpr)),
    ('Expr,       Vector('Set)),
    ('Expr,       Vector('List)),
    ('Expr,       Vector('MathExpr)),
    ('Set,        Vector('LCBRACK, 'RCBRACK)),
    ('Set,        Vector('LCBRACK, 'SetIn, 'RCBRACK)),
    ('SetIn,      Vector('SetIn, 'COMMA, 'Expr)),
    ('SetIn,      Vector('Expr)),
    ('List,       Vector('LSBRACK, 'RSBRACK)),
    ('List,       Vector('LSBRACK, 'ListIn, 'RSBRACK)),
    ('ListIn,     Vector('ListIn, 'COMMA, 'Expr)),
    ('ListIn,     Vector('Expr)),
    ('MathExpr,   Vector('MathExpr, 'PLUS, 'Term)),
    ('MathExpr,   Vector('MathExpr, 'MINUS, 'Term)),
    ('MathExpr,   Vector('Term)),
    ('Term,       Vector('Term, 'STAR, 'Factor)),
    ('Term,       Vector('Term, 'SLASH, 'Factor)),
    ('Term,       Vector('Factor)),
    ('Factor,     Vector('LRBRACK, 'MathExpr, 'RRBRACK)),
    ('Factor,     Vector('Factor, 'BANG)),
    ('Factor,     Vector('Factor, 'POW, 'Factor)),
    ('Factor,     Vector('WORD)),
    ('Factor,     Vector('FnCall)),
    ('Factor,     Vector('INT)),
    ('Factor,     Vector('FLOAT)),
    ('FnCall,     Vector('WORD, 'LRBRACK, 'AParams, 'RRBRACK)),
    ('FParams,    Vector('FParams, 'COMMA, 'WORD)),
    ('FParams,    Vector('WORD)),
    ('AParams,    Vector('AParams, 'COMMA, 'Expr)),
    ('AParams,    Vector('Expr))
  )

  def getStartingRules(): Set[Rule] = {
    rules.collect{ case rule @ ('Statement, v: Vector[Symbol]) => rule }
  }

  def start(): Symbol = {
    'Statement
  }

}
