//=============================================================================
// util/package.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

//=============================================================================
// util package object
//
// Collection of arbitrary utility functions
//
package object util {
  
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
