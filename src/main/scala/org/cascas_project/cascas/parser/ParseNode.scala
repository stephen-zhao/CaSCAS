//=============================================================================
// ParseNode.scala : CaSCAS Project
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

  // Protected field to represent type. When parsed, the node will not have
  // a type. To provide it with a type, use the withType method. Once given a
  // type, the node becomes immutable in type.
  protected var etype: EvaluatedTypeEnum = UnTyped
  private var isEtypeMutable: Boolean = true
  def withType(t: EvaluatedTypeEnum): ParseNodeLike = {
    if (isEtypeMutable) {
      this.etype = t
      this.isEtypeMutable = false
      this
    }
    else {
      throw new Exception("etype is not mutable. Can only set type once.")
    }
  }
  def typesAs(): EvaluatedTypeEnum = { this.etype }

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

sealed abstract class ParseNode extends ParseNodeLike {
}

final case class TerminalNode(
  val kind:  Symbol,
  val token: Token,
) extends ParseNode with TerminalNodeLike {
  def toRepr(): String = token.lexeme
}

final case class UnaryOperatorNode(
  val kind:     Symbol,
  val operator: ParseNode,
  val operand:  ParseNode
) extends ParseNode with NonTerminalNodeLike {
  def toRepr(): String = f"(${operator.toRepr} ${operand.toRepr})"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.operator
    case 1 => this.operand
    case x => this.handleGetBadIndex(x)
  }
}

final case class BinaryOperatorNode(
  val kind:     Symbol,
  val operator: ParseNode,
  val operands: (ParseNode, ParseNode),
) extends ParseNode with NonTerminalNodeLike {
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
  val assignee: ParseNode,
  val assigned: ParseNode,
) extends ParseNode with NonTerminalNodeLike {
  def toRepr(): String = f"let ${assignee.toRepr} := ${assigned.toRepr}"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.assignee
    case 1 => this.assigned
    case x => this.handleGetBadIndex(x)
  }
}

final case class ReAssignNode(
  val kind:     Symbol,
  val assignee: ParseNode,
  val assigned: ParseNode,
) extends ParseNode with NonTerminalNodeLike {
  def toRepr(): String = f"${assignee.toRepr} := ${assigned.toRepr}"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.assignee
    case 1 => this.assigned
    case x => this.handleGetBadIndex(x)
  }
}

final case class OperatorAssignNode(
  val kind:     Symbol,
  val assignee: ParseNode,
  val params:   ParseNode,
  val assigned: ParseNode,
) extends ParseNode with NonTerminalNodeLike {
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
  val wrapped: ParseNode,
) extends ParseNode with NonTerminalNodeLike {
  def toRepr(): String = wrapped.toRepr
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.wrapped
    case x => this.handleGetBadIndex(x)
  }
}

final case class StatementsNode(
  val kind:       Symbol,
  val statements: Vector[ParseNode],
) extends ParseNode with NonTerminalNodeLike {
  def toRepr(): String = statements.mkString("; ")
  def get(i: Int): ParseNodeLike = statements.lift(i) match {
    case Some(node) => node
    case None => this.handleGetBadIndex(i)
  }
}

final case class LambdaNode(
  val kind:   Symbol,
  val params: ParseNode,
  val body:   ParseNode,
) extends ParseNode with NonTerminalNodeLike {
  def toRepr(): String = '\u03bb'.toString + f"(${params.toRepr}).(${body.toRepr})"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.params
    case 1 => this.body
    case x => this.handleGetBadIndex(x)
  }
}

final case class SequenceNode(
  val kind:     Symbol,
  val sequence: Vector[ParseNode],
) extends ParseNode with NonTerminalNodeLike {
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
  val predicate: ParseNode,
  val body:      ParseNode,
) extends ParseNode with NonTerminalNodeLike {
  def toRepr(): String = f"while(${predicate.toRepr})(${body.toRepr})"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.predicate
    case 1 => this.body
    case x => this.handleGetBadIndex(x)
  }
}

final case class ForNode(
  val kind:       Symbol,
  val iterator:   ParseNode,
  val collection: ParseNode,
  val body:       ParseNode,
) extends ParseNode with NonTerminalNodeLike {
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
  val test:      ParseNode,
  val whenTrue:  ParseNode,
  val whenFalse: ParseNode,
) extends ParseNode with NonTerminalNodeLike {
  def toRepr(): String = f"if(${test.toRepr})(${whenTrue.toRepr})(${whenFalse.toRepr})"
  def get(i: Int): ParseNodeLike = i match {
    case 0 => this.test
    case 1 => this.whenTrue
    case 2 => this.whenFalse
    case x => this.handleGetBadIndex(x)
  }
}
