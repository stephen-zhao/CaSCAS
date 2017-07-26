//=============================================================================
// shared/Enumerated.scala : CaSCAS Project
//=============================================================================
// 
// Use this package to create enumerated types with typical enumeration 
// behaviour. Some example client code is given below. It is, unfortunately,
// very verbose.
//
// e.g.
//
// sealed abstract class Week extends Enumerated.OrderedValue[Week] {
//   def enumType = Week
// }
// final object Week extends Enumerated.OrderedType[Week] {
//   val enum = Vector(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)
// }
// final case object Sunday extends Week {}
// final case object Monday extends Week {}
// final case object Tuesday extends Week {}
// final case object Wednesday extends Week {}
// final case object Thursday extends Week {}
// final case object Friday extends Week {}
// final case object Saturday extends Week {}
//
package org.cascas_project.cascas.shared

object Enumerated {

  trait Type[E <: Value[E]] {
    val enum: Vector[E]
    def enumByValue: Map[Value[E], Int] = enum.zipWithIndex.toMap
    def apply(i: Int): E = enum.lift(i).getOrElse(throw new OutOfRangeException)
    def apply(e: Value[E]): Int = enumByValue(e)
    def cardinality = enum.length
  }

  trait Value[E <: Value[E]] extends Product with Serializable {
    def enumType: Type[E]
    def name: String = this.getClass.getName.toString.stripSuffix("$")
  }

  trait OrderedValue[E <: OrderedValue[E]] extends Value[E] {
    def pred: E = enumType(enumType(this) - 1)
    def succ: E = enumType(enumType(this) + 1)
    def predRot: E = 
      enumType((enumType(this) + enumType.cardinality - 1) % enumType.cardinality)
    def succRot: E =
      enumType((enumType(this) + 1) % enumType.cardinality)
    def toInt: Int = enumType(this)
  }

  class OutOfRangeException extends Exception {}

}
