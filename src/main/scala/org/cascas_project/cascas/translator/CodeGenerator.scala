//=============================================================================
// translator/CodeGenerator.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.translator

//=============================================================================

import scala.annotation.tailrec
import org.cascas_project.cascas.lang._
import org.cascas_project.cascas.lang.liro._
import org.cascas_project.cascas.parsetree._
import org.cascas_project.cascas.token._

class CodeGenerator {


  def generateLIRObject(parseTree: ParseNodeLike): Object = {

    parseTree match {

      // 1. INT kind TerminalNode becomes a RationalNumber Literal
      case TerminalNode('INT, n @ IntegerToken(_,_)) => this.getRationalNumberFromIntegerToken(n)

      // 2. FLOAT kind TerminalNode becomes a RationalNumber Literal
      case TerminalNode('FLOAT, x @ DecimalToken(_,_)) => this.getRationalNumberFromDecimalToken(x)

      // 3. other kind TerminalNode (keywords, key symbols, etc) becomes an Identifier
      case TerminalNode(_, other) => Identifier(other.lexeme)

      // 4. all kind UnaryOperatorNode becomes ApplyExpr with flattened parameter list
      case UnaryOperatorNode(kind, operator, operand) => {
        ApplyExpr(this.generateLIRObject(operator), operand match {
          // 4.1. when there is a parameter list longer than length 1
          case SequenceNode('AParams, seq) => seq.map(this.generateLIRObject(_))
          // 4.2. when there is only one parameter
          case node => Vector[liro.Object](this.generateLIRObject(node))
        })
      }

      // 5. MathExpr kind BinaryOperatorNode with PLUS operator or
      //    Term     kind BinaryOperatorNode with STAR operator
      //                                     becomes ApplyExpr
      //                                     with operator (Identifier("+")) or
      //                                     with operator (Identifier("*"))    and
      //                                     with flattened parameter ListExpr as the only parameter
      //                                     (if nested operators of the same kind)
      case BinaryOperatorNode(kind, operator @ TerminalNode(opKind,_), operands)
           if (kind == 'MathExpr && opKind == 'PLUS) || (kind == 'Term && opKind == 'STAR) => {

        // Function to do processing on flattening the left operand into the parameter list
        def processSameOperand1(): Vector[liro.Object] = {
          // Generate operand1's LIR Object to extract the operands that need promoting
          this.generateLIRObject(operands._1) match {
            // Extract the operands
            case ApplyExpr(_, Vector(ListExpr(leftOperands))) => operands._2 match {
              // 5.1.1. right operand kind == currently processing kind and
              //        right operator == current operator, so do flattening work on it too
              // CASE: Both left and right operands are the same kind, so flatten both
              //       and append them to each other
              case BinaryOperatorNode(kindOperandRight, TerminalNode(kindOperatorRight,_), _)
                   if kindOperandRight == kind && kindOperatorRight == opKind => {
                // Generate operand2's LIR Object to extract the operands that need promoting
                this.generateLIRObject(operands._2) match {
                  // Extract the operands and do the append
                  case ApplyExpr(_, Vector(ListExpr(rightOperands))) => leftOperands ++ rightOperands
                  // Error in extraction
                  case other => throw new Exception("ERROR") //TODO
                }
              }
              // 5.1.2. right operand kind =/= currently processing kind and
              //        right operator =/= current operator, then
              // CASE: Left Operand is same, right is not, so flatten only the left operand
              case operand2 => leftOperands :+ this.generateLIRObject(operand2)
            }
            // Error in extraction
            case other => throw new Exception("ERROR") //TODO
          }
        }

        ApplyExpr(this.generateLIRObject(operator), Vector(ListExpr(operands._1 match {
          // 5.1 left operand kind == currently processing kind and
          //     left operator == current operator, so do flattening work
          case BinaryOperatorNode(kindOperandLeft, TerminalNode(kindOperatorLeft, _), _)
               if kindOperandLeft == kind && kindOperatorLeft == opKind => processSameOperand1()
          // 5.2 left operand kind =/= currently processing kind or
          //     left operator =/= current operator, then
          case operand1 => operands._2 match {
            // 5.2.1. right operand kind == currently processing kind, so do flattening work
            // CASE: Right Operand is same, left is not, so flatten only the right operand
            case BinaryOperatorNode(kindOperandRight, TerminalNode(kindOperatorRight, _), _)
                 if kindOperandRight == kind && kindOperatorRight == opKind => {
              // Generate operand2's LIR Object to extract the operands that need promoting
              this.generateLIRObject(operands._2) match {
                // Extract the operands and do the flatten
                case ApplyExpr(_, Vector(ListExpr(rightOperands))) => this.generateLIRObject(operand1) +: rightOperands
                // Error in extraction
                case other => throw new Exception("ERROR") //TODO
              }
            }
            // 5.2.2. right operand kind =/= currently processing kind and
            //        right operator =/= current operator, then
            // CASE: Both operands are not the same kind, so just make a length-2 parameter list
            //       with the left and right operands
            case operand2 => Vector[liro.Object](
              this.generateLIRObject(operand1),
              this.generateLIRObject(operand2)
            )
          }
        })))
      }

      // 6. other kind BinaryOperatorNode becomes ApplyExpr with length-2 parameter list
      case BinaryOperatorNode(_, operator, operands) => {
        ApplyExpr(
          this.generateLIRObject(operator),
          Vector[liro.Object](
            this.generateLIRObject(operands._1),
            this.generateLIRObject(operands._2)
          )
        )
      }

      // 7. Assign kind AssignNode becomes
        
    } 
  }



  def getRationalNumberFromDecimalToken(dt: DecimalToken): RationalNumber = {

    @tailrec
    def parseWholePartRec(s: List[Char], w: List[Char] = List()): (String, String) = {
      s.headOption match {
        case None => (w.reverse.toString, "")
        case Some('_') => parseWholePartRec(s.tail, w)
        case Some('.') => parseFractionalPartRec(s.tail, w)
        case Some(x: Char) => parseWholePartRec(s.tail, x :: w)
      }
    }

    @tailrec
    def parseFractionalPartRec(
      s: List[Char],
      w: List[Char],
      f: List[Char] = List()
    ): (String, String) = {
      s.headOption match {
        case None => (w.reverse.toString, f.reverse.toString)
        case Some('_') => parseFractionalPartRec(s.tail, w, f)
        case Some(x: Char) => parseFractionalPartRec(s.tail, w, x :: f)
      }
    }

    parseWholePartRec(dt.lexeme.toList) match {
      case (w, f) => {
        val numerator = BigInt(w ++ f)
        val denominator = BigInt(10).pow((w ++ f).length)
        RationalNumber(numerator, denominator)
      }
    }

  }

  def getRationalNumberFromIntegerToken(it: IntegerToken): RationalNumber = {
    RationalNumber(BigInt(it.lexeme.filter(_ != '_')))
  }

}
