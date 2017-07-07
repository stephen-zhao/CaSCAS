//=============================================================================
// MathObj.scala : CaSCAS Project
//=============================================================================
package org.cascas_project.cascas

import scala.collection.mutable
import org.cascas_project.cascas.scope._

trait Algebraic {}


abstract class MathObj {
	def flatten (type :Symbol) = {
	  this
	}
}

/*
generic free in template:

def free (id : Identifier) : Boolean = {
    for param in params {
	    if not param.free(id)
		return false
	}
	if current node is a id or num {
	    if lookup (curr.name()) == id.name
		return false
	}
	return true
}

*/

case class Function(
  fn: String,
  parameters: MutableList[MathObj],
  context: Scope
  
) extends MathObj with Algebraic {
	
  def param (i : Int) : MathObj = {
	parameters(i)
  }
}

case class Addition (
  summands: MutableList[Algebraic]
) extends MathObj with Algebraic{
	
	def operand (i : Int) : Algebraic = {
	  summands(i)
	}
	
	override def flatten(type: Symbol) : MutableList[MathObj] = {
	  val terms = new MutableList[MathObj]
	  type match {
	    case 'PLUS =>
		  summands.foreach (((i) => ( 
			terms ++ i.flatten('PLUS)
			)))
		  terms
		case 'STAR =>
		   terms ++ this
		   terms
	  }
	}
}

case class Multiplication(
  terms: MutableList[Algebraic]
) extends MathObj with Algebraic{

  def operand (i : Int) : Algebraic = {
	terms(i)
  }
  
  override def flatten(type: Symbol) : MutableList[MathObj] = {
	  val terms = new MutableList[MathObj]
	  type match {
	    case 'STAR =>
		  summands.foreach (((i) => ( 
			terms ++ i.flatten('STAR)
			)))
		  terms
		case 'PLUS =>
		   terms ++ this
		   terms
	  }
	}
}

case class Exponentiation (
  base: Algebraic,
  exponent: Algebraic
) extends MathObj with Algebraic {

	def operand (i : Int) : Algebraic = {
	     if (i == 0) {
		     base
		 } else {
		     exponent
		 }
	}
	
	def getBase : Algebraic = {
	    base
	}
	
	def getExponent : Algebraic = {
	    exponent
	}
}

case class Number (
  value: Int
) extends MathObj with Algebraic{
	
	def getValue : Int = {
	    value
	}
}

case class Identifier(
  name : String,
  context: Scope
) extends MathObj with Algebraic{
	
	def getName : String = {
	    name
	}
}

case class Division(
  numerator: Algebraic,
  denominator: Algebraic
) extends MathObj with Algebraic {
	
	def operand (i : Int) : Algebraic = {
	     if (i == 0) {
		     numerator
		 } else {
		     denominator
		 }
	}
	
	def getNumerator : Algebraic = {
	    numerator
	}
	
	def getDenominator : Algebraic = {
	    denominator
	}
}
