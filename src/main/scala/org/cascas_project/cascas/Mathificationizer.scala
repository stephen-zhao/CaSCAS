//=============================================================================
// Mathificationizer.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

import scala.collection.mutable
import org.cascas_project.cascas.scope._
import org.cascas_project.cascas.mathObj._

//=============================================================================
// Mathificationizer class
//
// Converts a parsernode tree into a math tree.

class Mathificationizer {
  private var context: Scope
	
  def mathifyRec(p : ParseNode) : = MathObj {
    p match {
	  case NonTerminalNode (rep, derivs) =>
	    rep match {
		  case 'PLUS =>
			val summands = new MutableList[MathObj]
		    derivs.foreach (((i) => ( 
			  summands ++ mathifyRec(i).flatten('PLUS)
			)))
			val mathNode = new Addition(summands)
		  case 'MINUS =>
		    val summands = new MutableList[MathObj]
			val summands2 = new MutableList[MathObj]
		    derivs.foreach (((i) => ( 
			  summands ++ mathifyRec(i).flatten('PLUS)
			)))
			val minus1 = new Number(-1)
			val timesM1 = new Multiplication(minus1, summands(1))
			summands2 ++ summands(0)
			summands2 ++ timesM1.flatten('TIMES)
			val mathNode = new Addition(summands2)
		  case 'STAR =>
			val terms = new MutableList[MathObj]
		    derivs.foreach (((i) => ( 
			  terms ++ mathifyRec(i).flatten('TIMES)
			)))
			val mathNode = new Multiplication(terms)
		  case 'SLASH =>
			val terms = new MutableList[MathObj]
			val terms2 = new MutableList[MathObj]
		    derivs.foreach (((i) => ( 
			  terms ++ mathifyRec(i).flatten('TIMES)
			)))
			val minus1 = new Number(-1)
			val timesM1 = new Exponentiation(minus1, summands(1))
			terms2 ++ terms(0)
			terms2 ++ timesM1
			val mathNode = new Multiplication(terms2)
		  case 'BANG =>
		    val param = mathifyRec(derivs(0))
			val mathNode = new Function("factorial", param, context)
		  case _ =>
		    throw
		}
	  case TerminalNode (rep, token) =>
	    rep match {
		  case 'INT =>
		    val mathNode = new Number(token)
		  case 'FLOAT => 
		  case 'WORD =>
		    val mathNode = new Identifier(token, context)
		  case _ =>
		    throw
		}
	}
  }
}