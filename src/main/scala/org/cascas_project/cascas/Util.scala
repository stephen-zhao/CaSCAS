//=============================================================================
// Util.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

//=============================================================================
// Util object
//
// Collection of arbitrary utility functions
//
object Util {
  
  def toInt(s: String): Option[Int] = {
    try {
      
      Some(s.toInt)
    }
    catch {
      case e: Throwable => None
    }
  }

  def toDouble(s: String): Option[Double] = {
    try {

      Some(s.toDouble)
    }
    catch {
      case e: Throwable => None
    }
  }

}
