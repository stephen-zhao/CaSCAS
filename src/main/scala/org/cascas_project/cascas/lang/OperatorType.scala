package org.cascas_project.cascas.lang

case class OperatorType(args: FormalParamList, ret: TypeIdentifier) extends TypeIdentifier {

  override def toString(): String = args.map{ case (k, v) => f"($k : $v)" }.mkString(" -> ")

}

object OperatorType {
    
  // Internal builder class
  class Builder private[OperatorType](args: FormalParamList = FormalParamList.empty) {
    def apply(name: Identifier, tpe: TypeIdentifier): Builder = {
      new Builder(this.args + (name -> tpe))
    }
    def apply(tpe: TypeIdentifier): OperatorType = {
      new OperatorType(this.args, tpe)
    }
  }

  // Add a formal parameter to the OperatorType
  def apply(name: Identifier, tpe: TypeIdentifier): Builder = {
    new Builder()(name, tpe)
  }

  // Add a return type to the OperatorType
  def apply(tpe: TypeIdentifier): OperatorType = {
    new Builder()(tpe)
  }

}
