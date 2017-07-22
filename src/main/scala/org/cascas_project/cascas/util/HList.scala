//=============================================================================
// util/HList.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.util

//=============================================================================
// HList type generics
//
// Generics, traits, and classes to allow for heterogeneous list types
//
object HList {

  sealed trait HList

  final class HNil extends HList {
    def ::[T](v: T) = HCons(v, this)
  }

  final case class HCons[H, T <: HList](head : H, tail : T) extends HList {
    def ::[T](v: T) = HCons(v, this)
  }

  val HNil = new HNil()

  type ::[H, T <: HList] = HCons[H, T]

}
