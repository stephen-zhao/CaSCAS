//=============================================================================
// parser/Item.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

//=============================================================================

case class Item(
  lhs: Symbol,
  rhs: Vector[Symbol],
  pos: Int = 0
) {
  
  def nextSymbol: Option[Symbol] = {
    if (this.isAtEnd)
      None
    else
      Some(this.rhs(this.pos))
  }

  def advanceMarker: Item = {
    Item(this.lhs, this.rhs, this.pos + 1)
  }

  def isAtEnd: Boolean = {
    this.pos == this.rhs.length
  }

}
