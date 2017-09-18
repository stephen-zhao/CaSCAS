//=============================================================================
// parsetree/ParseNodes.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parsetree

//=============================================================================

import org.cascas_project.cascas.token._

//=============================================================================

//== PARSE NODE LIKE ==========================================================
sealed trait ParseNodeLike {

  // A toString that can be used to give a useful representation of the node
  // for human eyes and does not override the default toString, which may still
  // be used to convert the node to a canonical scala string representation.
  def toRepr: String

  // The kind of node is based on the context-free grammar used to parse the
  // node. All valid kinds are listed in the CaSCAS CFG definition.
  // TODO: a way to enforce type-correctness of this at compile-time
  val kind: Symbol

}

//== TERMINAL NODE LIKE =======================================================
sealed trait TerminalNodeLike extends ParseNodeLike {

  // The internal token
  val token: Token

}

//== NON TERMINAL NODE LIKE ===================================================
sealed trait NonTerminalNodeLike extends ParseNodeLike {
  
  // A more generic return-the-ith-child method, implemented per case class
  def get(i: Int): ParseNodeLike

  // A exception-safe version of get, defined based on get
  def getOption(i: Int): Option[ParseNodeLike] = {
    try {
      Some(this.get(i))
    }
    catch {
      case e: IndexOutOfBoundsException => None
    }
  }

  protected def handleGetBadIndex(i: Int): Nothing = {
    throw new IndexOutOfBoundsException(
      f"NonTerminalNodeLike#get($i), invalid index!"
    )
  }

}

//== TERMINAL NODE ============================================================
final case class TerminalNode(
  kind:  Symbol,
  token: Token,
) extends TerminalNodeLike {
  def toRepr: String = token.lexeme
}

//== UNARY OPERATOR NODE ======================================================
final case class UnaryOperatorNode(
  kind:     Symbol,
  operator: ParseNodeLike,
  operand:  ParseNodeLike
) extends NonTerminalNodeLike {
  def toRepr: String = f"(${operator.toRepr} ${operand.toRepr})"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.operator
    case 1 => this.operand
    case x => this.handleGetBadIndex(x)
  }
}

//== BINARY OPERATOR NODE =====================================================
final case class BinaryOperatorNode(
  kind:     Symbol,
  operator: ParseNodeLike,
  operands: (ParseNodeLike, ParseNodeLike),
) extends NonTerminalNodeLike {
  def toRepr: String = {
    f"(${operator.toRepr} ${operands._1.toRepr} ${operands._2.toRepr})"
  }
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.operator
    case 1 => this.operands._1
    case 2 => this.operands._2
    case x => this.handleGetBadIndex(x)
  }
}

//== ASSIGN NODE ==============================================================
final case class AssignNode(
  kind:     Symbol,
  assignee: ParseNodeLike,
  assigned: ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr: String = f"let ${assignee.toRepr} := ${assigned.toRepr}"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.assignee
    case 1 => this.assigned
    case x => this.handleGetBadIndex(x)
  }
}

//== REASSIGN NODE ============================================================
final case class ReAssignNode(
  kind:     Symbol,
  assignee: ParseNodeLike,
  assigned: ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr: String = f"${assignee.toRepr} := ${assigned.toRepr}"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.assignee
    case 1 => this.assigned
    case x => this.handleGetBadIndex(x)
  }
}

//== OPERATOR ASSIGN NODE =====================================================
final case class OperatorAssignNode(
  kind:     Symbol,
  assignee: ParseNodeLike,
  params:   ParseNodeLike,
  assigned: ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr: String = {
    f"let ${assignee.toRepr}(${params.toRepr}) := ${assigned.toRepr}"
  }
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.assignee
    case 1 => this.params
    case 2 => this.assigned
    case x => this.handleGetBadIndex(x)
  }
}

//== WRAPPER NODE =============================================================
final case class WrapperNode(
  kind:    Symbol,
  wrapped: ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr: String = wrapped.toRepr
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.wrapped
    case x => this.handleGetBadIndex(x)
  }
}

//== STATEMENTS NODE ==========================================================
final case class StatementsNode(
  kind:       Symbol,
  statements: Vector[ParseNodeLike],
) extends NonTerminalNodeLike {
  def toRepr: String = statements.mkString("; ")
  def get(i: Int): ParseNodeLike = statements.lift(i) match {
    case Some(node) => node
    case None => this.handleGetBadIndex(i)
  }
}

//== LAMBDA NODE ==============================================================
final case class LambdaNode(
  kind:   Symbol,
  params: ParseNodeLike,
  body:   ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr: String = {
    '\u03bb'.toString + f"(${params.toRepr}).(${body.toRepr})"
  }
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.params
    case 1 => this.body
    case x => this.handleGetBadIndex(x)
  }
}

//== SEQUENCE NODE ============================================================
final case class SequenceNode(
  kind:     Symbol,
  sequence: Vector[ParseNodeLike],
) extends NonTerminalNodeLike {
  def toRepr: String = {
    kind match {
      case 'ListIn => "[" + sequence.mkString(", ") + "]"
      case 'SetIn  => "{" + sequence.mkString(", ") + "}"
      case _       =>       sequence.mkString(", ")
    }
  }
  def get(i: Int): ParseNodeLike = sequence.lift(i) match {
    case Some(node) => node
    case None => this.handleGetBadIndex(i)
  }
}

//== WHILE NODE ===============================================================
final case class WhileNode(
  kind:      Symbol,
  predicate: ParseNodeLike,
  body:      ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr: String = f"while(${predicate.toRepr})(${body.toRepr})"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.predicate
    case 1 => this.body
    case x => this.handleGetBadIndex(x)
  }
}

//== FOR NODE =================================================================
final case class ForNode(
  kind:       Symbol,
  iterator:   ParseNodeLike,
  collection: ParseNodeLike,
  body:       ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr: String = {
    f"for ${iterator.toRepr} in ${collection.toRepr} (${body.toRepr})"
  }
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.iterator
    case 1 => this.collection
    case 2 => this.body
    case x => this.handleGetBadIndex(x)
  }
}

//== IF NODE ==================================================================
final case class IfNode(
  kind:      Symbol,
  test:      ParseNodeLike,
  whenTrue:  ParseNodeLike,
  whenFalse: ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr: String = {
    f"if(${test.toRepr})(${whenTrue.toRepr})(${whenFalse.toRepr})"
  }
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.test
    case 1 => this.whenTrue
    case 2 => this.whenFalse
    case x => this.handleGetBadIndex(x)
  }
}
