//=============================================================================
// Mathificationizer.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

import org.cascas_project.cascas.scope._
import org.cascas_project.cascas.mathObj._

//=============================================================================
// Mathificationizer class
//
// Converts a parsernode tree into a math tree.

class Mathificationizer {
  private var context: Scope
	
  def mathifyRec(p : ParseNode, acc : MathObj) : = MathObj {
    p match {
	  case NonTerminalNode (rep, derivs) =>
	    rep match {
		  case 'PLUS =>
		    derivs.foreach (((i) => ( 
			  
			))) 
		  case 'MINUS =>
		  case 'STAR =>
		  case 'SLASH =>
		  case 'BANG =>
		  case 'AND =>
		  case 'OR =>
		  case 'NOT =>
		  case 'LE =>
		  case 'GE =>
		  case 'EQ =>
		  case 'NEQ =>
		  case 'LT =>
		  case 'GT =>
		  case _ =>
		}
	  case TerminalNode (rep, token) =>
	    rep match {
		  case 'INT =>
		  case 'FLOAT => 
		  case 'WORD =>
		  case _ =>
		}
	}
  }
}