package org.cascas_project.cascas.lang

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
