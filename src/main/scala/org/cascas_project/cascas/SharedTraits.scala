//=============================================================================
// SharedTraits.scala : CaSCAS
//=============================================================================

package org.cascas_project.cascas

//=============================================================================
// Imports
//

//=============================================================================
// Typed trait
//
// Objects that have a CaSCAS Type must implement this trait
//
trait Typed {
  def typeof: Symbol
}

//=============================================================================
// NameLike trait
//
// Objects that can be used as a name in a Scope must implement this trait
//
trait NameLike extends Typed {
}

//=============================================================================
// ValueLike trait
//
// Objects that can be values in a Scope must implement this trait
//
trait ValueLike extends Typed {

}
