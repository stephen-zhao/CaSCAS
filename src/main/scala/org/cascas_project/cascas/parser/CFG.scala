//=============================================================================
// CFG.scala : CaSCAS Project
//=============================================================================
package org.cascas_project.cascas.parser

object CFG {

  type Rule = (Symbol, Vector[Symbol])
  type RuleSet = Set[Rule]
  
  val nonterminals: Set[Symbol] = 
    Set('Program, 'Statements, 'Statement, 'Assign, 'ReAssign, 'Expr, 'Control,
        'IfControl, 'ElControl, 'WhileControl, 'ForControl, 'Collection,
        'Set, 'SetIn, 'List, 'ListIn, 'BoolExpr, 'BoolOrend, 'BoolAndend,
        'Relation, 'MathExpr, 'Term, 'Factor, 'FnCall, 'Num, 'FParams, 'AParams)
  
  val terminals: Set[Symbol] = 
    Set('LET, 'IF, 'ELSIF, 'ELSE, 'WHILE, 'FOR, 'IN,
        'WORD, 'INT, 'FLOAT, 'COMMA, 'ASSIGN, 'NEWLINE,
        'LRBRACK, 'RRBRACK, 'LCBRACK, 'RCBRACK, 'LSBRACK, 'RSBRACK, 
        'OR, 'AND, 'EQ, 'NEQ, 'GT, 'LT, 'GTE, 'LTE,
        'PLUS, 'MINUS, 'STAR, 'SLASH, 'BANG, 'POW)

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
    ('Program,    Vector('Statements)),

    ('Statements, Vector('Statements, 'NEWLINE, 'Statement)),
    ('Statements, Vector('Statement)),

    ('Statement,  Vector('Assign)),
    ('Statement,  Vector('ReAssign)),
    ('Statement,  Vector('Expr)),

    ('Assign,     Vector('LET, 'ReAssign)),
    ('ReAssign,   Vector('WORD, 'ASSIGN, 'Expr)),
    ('ReAssign,   Vector('WORD, 'LRBRACK, 'FParams, 'RRBRACK, 'ASSIGN, 'Expr)),

    ('Expr,       Vector('Control)),
    ('Expr,       Vector('Collection)),
    ('Expr,       Vector('BoolExpr)),
    ('Expr,       Vector('MathExpr)),

    ('Control,    Vector('IfControl)),
    ('Control,    Vector('WhileControl)),
    ('Control,    Vector('ForControl)),
    
    ('IfControl,  Vector('IF, 'LRBRACK, 'Expr, 'RRBRACK, 'LRBRACK, 'Statements, 'RRBRACK)),
    ('IfControl,  Vector('IF, 'LRBRACK, 'Expr, 'RRBRACK, 'LRBRACK, 'Statements, 'RRBRACK, 'ElControl)),
    ('ElControl,  Vector('ELSIF, 'LRBRACK, 'Expr, 'RRBRACK, 'LRBRACK, 'Statements, 'RRBRACK, 'ElControl)),
    ('ElControl,  Vector('ELSE, 'LRBRACK, 'Statements, 'RRBRACK)),

    ('WhileControl, Vector('WHILE, 'LRBRACK, 'Expr, 'RRBRACK, 'LRBRACK, 'Statements, 'RRBRACK)),
    ('ForControl, Vector('FOR, 'LRBRACK, 'WORD, 'IN, 'Collection, 'RRBRACK, 'LRBRACK, 'Statements, 'RRBRACK)),

    ('Collection, Vector('Set)),
    ('Collection, Vector('List)),

    ('Set,        Vector('LCBRACK, 'RCBRACK)),
    ('Set,        Vector('LCBRACK, 'SetIn, 'RCBRACK)),
    ('SetIn,      Vector('SetIn, 'COMMA, 'Expr)),
    ('SetIn,      Vector('Expr)),

    ('List,       Vector('LSBRACK, 'RSBRACK)),
    ('List,       Vector('LSBRACK, 'ListIn, 'RSBRACK)),
    ('ListIn,     Vector('ListIn, 'COMMA, 'Expr)),
    ('ListIn,     Vector('Expr)),

    ('BoolExpr,   Vector('BoolExpr, 'OR, 'BoolOrend)),
    ('BoolExpr,   Vector('BoolOrend)),

    ('BoolOrend,  Vector('BoolOrend, 'AND, 'BoolAndend)),
    ('BoolOrend,  Vector('BoolAndend)),

    ('BoolAndend, Vector('LRBRACK, 'BoolExpr, 'RRBRACK)),
    ('BoolAndend, Vector('Relation)),

    ('Relation,   Vector('MathExpr, 'EQ, 'MathExpr)),
    ('Relation,   Vector('MathExpr, 'NEQ, 'MathExpr)),
    ('Relation,   Vector('MathExpr, 'GT, 'MathExpr)),
    ('Relation,   Vector('MathExpr, 'LT, 'MathExpr)),
    ('Relation,   Vector('MathExpr, 'GE, 'MathExpr)),
    ('Relation,   Vector('MathExpr, 'LE, 'MathExpr)),

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
    rules.collect{ case rule @ ('Program, v: Vector[Symbol]) => rule }
  }

  def start(): Symbol = {
    'Program
  }

}
