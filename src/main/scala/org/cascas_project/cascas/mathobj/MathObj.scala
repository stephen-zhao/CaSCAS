//=============================================================================
// MathObj.scala : CaSCAS Project
//=============================================================================

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

class Function extends MathObj with Algebraic {
	private val fn: String
	private val params: scala.collection.mutable[MathObj]
	
	def operand (i : Int) : Algebraic = {
	     params[i]
	}
}

class Addition extends MathObj with Algebraic{
	private val params: scala.collection.mutable[Algebraic]
	
	def operand (i : Int) : Algebraic = {
	     summands[i]
	}
}

class Multiplication extends MathObj with Algebraic{
	private val terms: scala.collection.mutable[Algebraic]
	
	def operand (i : Int) : Algebraic = {
	     terms[i]
	}
}

class Exponentiation extends MathObj with Algebraic {
	private val base: Algebraic
	private val exponent: Algebraic

	def operand (i : Int) : Algebraic = {
	     if (i == 0) {
		     base
		 } else if (i == 1) {
		     exponent
		 }
	}
	
	def base : Algebraic = {
	    base
	}
	
	def exponent : Algebraic = {
	    exponent
	}
}

class Number extends MathObj with Algebraic{
	private val value: Int
	
	def value : Int = {
	    value
	}
}

class Identifier extends MathObj with Algebraic{
	val name : String
	
	def name : String = {
	    name
	}
}

class Division extends mathObj with Algebraic {
	val numerator: Algebraic
	val denominator: Algebraic
	
	def operand (i : Int) : Algebraic = {
	     if (i == 0) {
		     numerator
		 } else if (i == 1) {
		     denominator
		 }
	}
	
	def numerator : Algebraic = {
	    numerator
	}
	
	def denominator : Algebraic = {
	    denominator
	}
}
