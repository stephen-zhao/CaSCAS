//=============================================================================
// lang/Context.scala : CaSCAS Project
//=============================================================================
// A Context is used in the runtime to represent a mapping from identifiers
// (names) to either types or type-value pairs.
//
// A mapping from an identifier to a type represents a declaration of a name
// that hasn't been assigned. e.g. a formal parameter definition.
//
// A mapping from an identifier to a type-value pair (TypedObject) represents
// a name that was assigned a object (and which has a type association).

package org.cascas_project.cascas.lang

//=============================================================================

import org.cascas_project.cascas.lang.liro.Identifier

//=============================================================================

case class Context (
  m: Map[Identifier, ContextValue] = Map[Identifier, ContextValue]()
) {

  def get(key: Identifier): Option[ContextValue] = m.get(key)

  def iterator: Iterator[(Identifier, ContextValue)] = m.iterator

  def +(kvp: (Identifier, ContextValue)): Context = Context(this.m + kvp)

  def -(key: Identifier): Context = Context(this.m - key)

  def toMap(): Map[Identifier, ContextValue] = m

  override def toString(): String = {
    "Context containing {\n" +
    this.m.map{ case (k, v) => f"$k : $v" }.mkString("\n") +
    "\n}"
  }

  // ConsolidatedWith is used to incorporate context changes gathered in a
  // ContextMutationSet into a Context. ContextMutationSet breaks down the
  // changes into three components: introductions, assignments, and
  // reassignments, and each of these must be handled appropriately.
  def consolidatedWith(ctxDelta: ContextMutationSet): Context = {
    val intros:    Map[Identifier, ContextValue] = ctxDelta.getIntroductions
    val assigns:   Map[Identifier, ContextValue] = ctxDelta.getAssignments
    val reassigns: Map[Identifier, ContextValue] = ctxDelta.getReassignments

    val intermediateM: Map[Identifier, ContextValue] = this.m ++ intros ++ assigns

    val filteredReassigns = reassigns.filter {
      case (k, v) => !(intermediateM contains k)
    }

    Context(intermediateM ++ filteredReassigns)
  }

  def :+(ctxDelta: ContextMutationSet): Context = consolidatedWith(ctxDelta)

}

object Context {
  val empty = Context()
}

trait ContextValue {}
