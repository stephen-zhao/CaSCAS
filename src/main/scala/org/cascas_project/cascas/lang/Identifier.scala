package org.cascas_project.cascas.lang

case class Identifier(name: String) extends TypeIdentifier with Object {

  override def toString(): String = name

}

object Identifier {

  def apply(name: Symbol): Identifier = {
    Identifier(name.toString.drop(1))
  }

}

