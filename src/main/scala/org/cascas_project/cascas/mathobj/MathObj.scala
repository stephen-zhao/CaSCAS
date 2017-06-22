//=============================================================================
// MathObj.scala : CaSCAS Project
//=============================================================================

trait Algebraic {}


abstract class MathObj {
	
}

class FunctionNode extends MathObj with Algebraic {
	val fn: String,
	val params: scala.collection.mutable[MathObj]
}

class AddNode extends MathObj with Algebraic{
	val summands: scala.collection.mutable[Algebraic]
}

class MultNode extends MathObj with Algebraic{
	val terms: scala.collection.mutable[Algebraic]
}

class ExpNode extends MathObj with Algebraic {
	val terms: scala.collection.mutable[Algebraic]
}

class NumNode extends MathObj with Algebraic{
	val value: int
}

class IdNode extends MathObj with Algebraic{
	val name: String
}

class Divnode extends mathObj with Algebraic {
	val numerator: Algebraic,
	val denominator: Algebraic
}