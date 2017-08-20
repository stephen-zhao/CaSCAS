//=============================================================================
// ParseNodeLike.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import org.cascas_project.cascas.Logger
import org.cascas_project.cascas.lang.EvaluatedType._
import org.cascas_project.cascas.tokens._

sealed trait ParseNodeLike {

  // A toString that can be used to give a useful representation of the node
  // for human eyes and does not override the default toString, which may still
  // be used to convert the node to a canonical scala string representation.
  def toRepr(): String

  // The kind of node is based on the context-free grammar used to parse the
  // node. All valid kinds are listed in the CaSCAS CFG definition.
  // TODO: a way to enforce type-correctness of this at compile-time
  val kind: Symbol

}

sealed trait TerminalNodeLike extends ParseNodeLike {

  // The internal token
  val token: Token

}

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
    throw new IndexOutOfBoundsException(f"NonTerminalNodeLike#get($i), invalid index!")
  }

}

final case class TerminalNode(
  val kind:  Symbol,
  val token: Token,
) extends TerminalNodeLike {
  def toRepr(): String = token.lexeme
}

final case class UnaryOperatorNode(
  val kind:     Symbol,
  val operator: ParseNodeLike,
  val operand:  ParseNodeLike
) extends NonTerminalNodeLike {
  def toRepr(): String = f"(${operator.toRepr} ${operand.toRepr})"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.operator
    case 1 => this.operand
    case x => this.handleGetBadIndex(x)
  }
}

final case class BinaryOperatorNode(
  val kind:     Symbol,
  val operator: ParseNodeLike,
  val operands: (ParseNodeLike, ParseNodeLike),
) extends NonTerminalNodeLike {
  def toRepr(): String = f"(${operator.toRepr} ${operands._1.toRepr} ${operands._2.toRepr})"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.operator
    case 1 => this.operands._1
    case 2 => this.operands._2
    case x => this.handleGetBadIndex(x)
  }
}

final case class AssignNode(
  val kind:     Symbol,
  val assignee: ParseNodeLike,
  val assigned: ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr(): String = f"let ${assignee.toRepr} := ${assigned.toRepr}"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.assignee
    case 1 => this.assigned
    case x => this.handleGetBadIndex(x)
  }
}

final case class ReAssignNode(
  val kind:     Symbol,
  val assignee: ParseNodeLike,
  val assigned: ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr(): String = f"${assignee.toRepr} := ${assigned.toRepr}"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.assignee
    case 1 => this.assigned
    case x => this.handleGetBadIndex(x)
  }
}

final case class OperatorAssignNode(
  val kind:     Symbol,
  val assignee: ParseNodeLike,
  val params:   ParseNodeLike,
  val assigned: ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr(): String = f"let ${assignee.toRepr}(${params.toRepr}) := ${assigned.toRepr}"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.assignee
    case 1 => this.params
    case 2 => this.assigned
    case x => this.handleGetBadIndex(x)
  }
}

final case class WrapperNode(
  val kind:    Symbol,
  val wrapped: ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr(): String = wrapped.toRepr
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.wrapped
    case x => this.handleGetBadIndex(x)
  }
}

final case class StatementsNode(
  val kind:       Symbol,
  val statements: Vector[ParseNodeLike],
) extends NonTerminalNodeLike {
  def toRepr(): String = statements.mkString("; ")
  def get(i: Int): ParseNodeLike = statements.lift(i) match {
    case Some(node) => node
    case None => this.handleGetBadIndex(i)
  }
}

final case class LambdaNode(
  val kind:   Symbol,
  val params: ParseNodeLike,
  val body:   ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr(): String = '\u03bb'.toString + f"(${params.toRepr}).(${body.toRepr})"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.params
    case 1 => this.body
    case x => this.handleGetBadIndex(x)
  }
}

final case class SequenceNode(
  val kind:     Symbol,
  val sequence: Vector[ParseNodeLike],
) extends NonTerminalNodeLike {
  def toRepr(): String = {
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

final case class WhileNode(
  val kind:      Symbol,
  val predicate: ParseNodeLike,
  val body:      ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr(): String = f"while(${predicate.toRepr})(${body.toRepr})"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.predicate
    case 1 => this.body
    case x => this.handleGetBadIndex(x)
  }
}

final case class ForNode(
  val kind:       Symbol,
  val iterator:   ParseNodeLike,
  val collection: ParseNodeLike,
  val body:       ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr(): String = f"for ${iterator.toRepr} in ${collection.toRepr} (${body.toRepr})"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.iterator
    case 1 => this.collection
    case 2 => this.body
    case x => this.handleGetBadIndex(x)
  }
}

final case class IfNode(
  val kind:      Symbol,
  val test:      ParseNodeLike,
  val whenTrue:  ParseNodeLike,
  val whenFalse: ParseNodeLike,
) extends NonTerminalNodeLike {
  def toRepr(): String = f"if(${test.toRepr})(${whenTrue.toRepr})(${whenFalse.toRepr})"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.test
    case 1 => this.whenTrue
    case 2 => this.whenFalse
    case x => this.handleGetBadIndex(x)
  }
}
