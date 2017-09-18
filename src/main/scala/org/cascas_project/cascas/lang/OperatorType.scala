//=============================================================================
// lang/OperatorType.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang

//=============================================================================

import org.cascas_project.cascas.lang.liro.Identifier

//=============================================================================

case class OperatorType(
  args: Vector[FormalParameter],
  ret:  TypeIdentifier
) extends TypeIdentifier {

  override def toString(): String = args.map {
   case FormalParameter(id, tpe, One) => f"($id : $tpe)"
   case FormalParameter(id, tpe, Many) => f"($id* : List($tpe))"
  }.mkString(" -> ")

}

object OperatorType {
    
  // Internal builder class
  class Builder private[OperatorType] {
    var args: Vector[FormalParameter] = Vector()
    var hasParamWithManyManyness: Boolean = false
    
    def apply(name: Identifier, tpe: TypeIdentifier, manyness: Manyness = One): Builder = {
      if (this.hasParamWithManyManyness) {
        throw new Exception("parameter being defined after a \"many\" parameter")
      }
      else if (manyness == Many) {
        this.hasParamWithManyManyness = true
      }
      this.args = this.args :+ FormalParameter(name, tpe, manyness)
      this
    }

    def apply(tpe: TypeIdentifier): OperatorType = {
      new OperatorType(this.args, tpe)
    }

  }

  // Add a formal parameter to the OperatorType
  def apply(name: Identifier, tpe: TypeIdentifier, manyness: Manyness = One): Builder = {
    new Builder()(name, tpe, manyness)
  }

  // Add a return type to the OperatorType
  def apply(tpe: TypeIdentifier): OperatorType = {
    new Builder()(tpe)
  }

}