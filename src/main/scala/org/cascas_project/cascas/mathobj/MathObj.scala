//=============================================================================
// MathObj.scala : CaSCAS Project
//=============================================================================

trait Addable {}

trait Multiplicable {}

trait Exponentiable {}

abstract class MathObj {
	
}

class FunctionNode extends MathObj with Addable,
									   Multiplicable,
									   Exponentiable {
	val fn: String,
	val params: scala.collection.mutable[MathObj]
}

class AddNode extends MathObj with Addable,
									Multiplicable,
									Exponentiable {
	val summands: scala.collection.mutable[Addable]
}

class MultNode extends MathObj with Addable,
									Multiplicable,
									Exponentiable {
	val terms: scala.collection.mutable[Multiplicable]
}

class ExpNode extends MathObj with Addable,
								   Multiplicable,
								   Exponentiable {
	val terms: scala.collection.mutable[Exponentiable]
}

class NumNode extends MathObj with Addable,
								   Multiplicable,
								   Exponentiable {
	val value: int
}

class IdNode extends MathObj with Addable,
								  Multiplicable,
								  Exponentiable {
	val name: String
}

class Divnode extends mathObj with Addable,
								   Multiplicable,
								   Exponentiable {
	val numerator: Multiplicable,
	val denominator: Multiplicable
}