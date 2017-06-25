//=============================================================================
// MathObj.scala : CaSCAS Project
//=============================================================================
import scala.collection.mutable


trait Algebraic {}


abstract class MathObj {
	
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
  parameters: List[MathObj]
) extends MathObj with Algebraic {
	
  def param (i : Int) : MathObj = {
	parameters(i)
  }
}

case class Addition (
  summands: List[Algebraic]
) extends MathObj with Algebraic{
	
	def operand (i : Int) : Algebraic = {
	  summands(i)
	}
}

case class Multiplication(
  terms: List[Algebraic]
) extends MathObj with Algebraic{

  def operand (i : Int) : Algebraic = {
	terms(i)
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
  name : String
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
