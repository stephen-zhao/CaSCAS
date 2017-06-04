package org.cascas_project.cascas.parser

case class Item(
  val lhs: Symbol,
  val rhs: Vector[Symbol],
  val pos: Int
) {
  
  def nextSymbol(): Option[Symbol] = {
    if (this.isAtEnd)
      None
    else
      Some(this.rhs(this.pos + 1))
  }

  def advanceMarker(): Item = {
    Item(this.lhs, this.rhs, this.pos + 1)
  }

  def isAtEnd(): Boolean = {
    this.pos + 1 == this.rhs.length
  }

}
