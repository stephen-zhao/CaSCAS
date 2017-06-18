//=============================================================================
// Scope.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

//=============================================================================
// Imports
//
import scala.collection.mutable.{Map => MMap}

//=============================================================================
// Scope class
//
// Represents a scope in the CaSCAS environment
//
class Scope extends MMap[NameLike, ValueLike] {
  
  var theMap: MMap[NameLike, ValueLike] = MMap[NameLike, ValueLike]()

  override def get(key: NameLike): Option[ValueLike] = {
    theMap.get(key)
  }
  override def iterator: Iterator[(NameLike,ValueLike)] = {
    theMap.iterator
  }
  override def -=(key: NameLike): Scope.this.type = {
    theMap -= key
    this
  }
  override def +=(kv: (NameLike, ValueLike)): Scope.this.type = {
    theMap += kv
    this
  }
}
