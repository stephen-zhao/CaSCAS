package org.cascas_project.cascas.lang

case class ContextGen[V] (
  m: Map[Identifier, V] = Map[Identifier, V]()
) extends Map[Identifier, V] {

  def get(key: Identifier): Option[V] = m.get(key)

  def iterator: Iterator[(Identifier, V)] = m.iterator

  def +[E >: V](kvp: (Identifier, V)): ContextGen[E] = ContextGen(this.m + kvp)

  def -(key: Identifier): ContextGen[V] = ContextGen(this.m - key)

  def toMap(): Map[Identifier, V] = m

  override def toString(): String = {
    "Context containing {\n" +
    this.map{ case (k, v) => f"$k : $v" }.mkString("\n") +
    "\n}"
  }

}

object Context {
  val empty = ContextGen[ContextValue]()
  def apply(
    m: Map[Identifier, ContextValue] = Map[Identifier, ContextValue]
  ) = ContextGen[ContextValue](m)
}

object FormalParamList {
  val empty = ContextGen[TypeIdentifier]()
  def apply(
    m: Map[Identifier, TypeIdentifier] = Map[Identifier, TypeIdentifier]
  ) = ContextGen[TypeIdentifier](m)
}

trait ContextValue {}
