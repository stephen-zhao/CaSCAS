//=============================================================================
// lang/liro/RationalNumber.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.liro

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.ContextMutationSet
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.TypeIdentifier

//=============================================================================

case class RationalNumber private (numerator: BigInt, denominator: BigInt) extends Literal {
  
  def *(that: RationalNumber): RationalNumber = {
    RationalNumber(this.numerator.*(that.numerator), this.denominator.*(that.denominator)).reduced()
  }

  def +(that: RationalNumber): RationalNumber = {
    if (this.denominator == that.denominator) {
      RationalNumber(this.numerator.+(that.numerator), this.denominator).reduced()
    }
    else {
      val newDenom = this.denominator./(this.denominator.gcd(that.denominator)).*(that.denominator)
      val thisFactor = newDenom./(this.denominator)
      val thatFactor = newDenom./(that.denominator)
      RationalNumber(this.numerator.*(thisFactor).+(that.numerator.*(thatFactor)), newDenom).reduced()
    }
  }

  def -(that: RationalNumber): RationalNumber = {
    this.+(that.unary_-())
  }

  def /(that: RationalNumber): RationalNumber = {
    this.*(that.reciprocal())
  }

  def unary_-(): RationalNumber = {
    RationalNumber(-this.numerator, this.denominator)
  }

  def reciprocal(): RationalNumber = {
    RationalNumber(this.denominator, this.numerator)
  }

  def reduced(): RationalNumber = {
    val reduceFactor = this.numerator.gcd(this.denominator)
    RationalNumber(this.numerator./(reduceFactor), this.denominator./(reduceFactor))
  }

  def <(that: RationalNumber): Boolean = {
    this.numerator.*(that.denominator) < that.numerator.*(this.denominator)
  }

  def >(that: RationalNumber): Boolean = {
    this.numerator.*(that.denominator) > that.numerator.*(this.denominator)
  }

  def <=(that: RationalNumber): Boolean = {
    this.numerator.*(that.denominator) <= that.numerator.*(this.denominator)
  }

  def >=(that: RationalNumber): Boolean = {
    this.numerator.*(that.denominator) >= that.numerator.*(this.denominator)
  }

  def equals(that: RationalNumber): Boolean = {
    this.numerator.*(that.denominator) == that.numerator.*(this.denominator)
  }

  def equals(that: BigInt): Boolean = {
    val reducedThis = this.reduced()
    reducedThis.denominator == 1 &&
    reducedThis.numerator == that
  }

  def isInteger: Boolean = {
    this.numerator.mod(this.denominator) == 0
  }

  def isWhole: Boolean = {
    val reducedThis = this.reduced()
    this.numerator.mod(this.denominator) == 0 &&
    reducedThis > RationalNumber.zero
  }

  def isNatural: Boolean = {
    val reducedThis = this.reduced()
    this.numerator.mod(this.denominator) == 0 &&
    reducedThis >= RationalNumber.zero
  }

  def eval(ctx: Context): Evaluation = {
    // Evaluating a rational number involves simply reducing it
    // Back propagate an empty context delta
    Evaluation(this.reduced(), ContextMutationSet.empty)
  }

  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = {
    tpe == Identifier("Number")
  }

  def inferType(ctx: Context): Option[TypeIdentifier] = {
    Some(Identifier("Number"))
  }

  def inferTheirTypes(
    ctx: Context,
    themToTheirMaybeTypes: Map[Identifier, Option[TypeIdentifier]]
  ): Map[Identifier, Option[TypeIdentifier]] = {
    themToTheirMaybeTypes
  }

  def toRepr(indentLevel: Int): String = {
    if (this.denominator == 1) {
      f"${this.numerator}"
    }
    else {
      f"${this.numerator}/${this.denominator}"
    }
  }

}

object RationalNumber {

  val zero: RationalNumber = RationalNumber(0, 1)

  def additiveIdentity: RationalNumber = zero

  val one: RationalNumber = RationalNumber(1, 1)

  def multiplicativeIdentity: RationalNumber = one

  def apply(numerator: BigInt, denominator: BigInt): RationalNumber = {
    if (denominator < 0) {
      new RationalNumber(numerator * -1, denominator * -1)
    } else {
      new RationalNumber(numerator, denominator)
    }
  }
  
  def apply(i: Int): RationalNumber = RationalNumber(i, 1)

  def apply(i: BigInt): RationalNumber = RationalNumber(i, 1)

}
