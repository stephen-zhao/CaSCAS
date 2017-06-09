package org.cascas_project.cascas.parser

object CFG {
  type Rule = (Symbol, Vector[Symbol])
  type RuleSet = Set[Rule]
  
  val nonterminals: Set[Symbol] = Set('Statement, 'Assign, 'Expr, 'Set, 'SetIn, 'MathExpr, 'Term, 'Factor, 'FnCall, 'Num, 'FParams, 'AParams)
  
  val terminals: Set[Symbol] = Set('WORD, 'INT, 'FLOAT, 'COMMA, 'LRBRACK, 'RRBRACK, 'LCBRACK, 'RCBRACK, 'ASSIGN, 'EQ, 'NEQ, 'GT, 'LT, 'GTE, 'LTE, 'PLUS, 'MINUS, 'STAR, 'SLASH, 'BANG, 'POW)

  //val terminals_: Set[Symbol] = terminals ++ Set('_, 'BOF, 'EOF)

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
    ('Expr,       Vector('MathExpr)),
    ('Expr,       Vector('Set)),
    ('Set,        Vector('LCBRACK, 'RCBRACK)),
    ('Set,        Vector('LCBRACK, 'SetIn, 'RCBRACK)),
    ('SetIn,      Vector('Expr, 'COMMA, 'SetIn)),
    ('SetIn,      Vector('Expr)),
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
