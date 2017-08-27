package org.cascas_project.cascas.lang

case class RationalNumber(numerator: BigInt, denominator: BigInt) extends Literal {

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

  def isInteger(): Boolean = {
    this.reduced().denominator == 1
  }

  def isWhole(): Boolean = {
    val reducedThis = this.reduced()
    reducedThis.denominator == 1 &&
    reducedThis > RationalNumber.zero
  }

  def isNatural(): Boolean = {
    val reducedThis = this.reduced()
    reducedThis.denominator == 1 &&
    reducedThis >= RationalNumber.zero
  }

  def eval(ctx: Context): Evaluation = {
    Evaluation(this.reduced(), ContextMutationSet.empty)
  }

  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = {
    if (tpe == Identifier("Number")) true
    else false
  }

  def inferType(ctx: Context): Option[TypeIdentifier] = {
    Some(Identifier("Number"))
  }

}

object RationalNumber {

  val zero = RationalNumber(0, 1)

  def apply(i: Int): RationalNumber = RationalNumber(i, 1)

}
