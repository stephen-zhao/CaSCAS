//=============================================================================
// translator/CodeGenerator.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.translator

//=============================================================================

import scala.annotation.tailrec
import org.cascas_project.cascas.Logger
import org.cascas_project.cascas.lang._
import org.cascas_project.cascas.lang.liro._
import org.cascas_project.cascas.parsetree._
import org.cascas_project.cascas.token._

class CodeGenerator {


  def generateLIRO(parseTree: ParseNodeLike): Object = {

    Logger.info('TRNSLTR, f"Generating LIRO from ParseNodeLike: ${parseTree}")

    parseTree match {

      // 1. INT kind TerminalNode becomes a RationalNumber Literal
      case TerminalNode('INT, it @ IntegerToken(_,_)) => {
        this.processIntegerToken(it)
      }

      // 2. FLOAT kind TerminalNode becomes a RationalNumber Literal
      case TerminalNode('FLOAT, dt @ DecimalToken(_,_)) => {
        this.processDecimalToken(dt)
      }

      // 3. other kind TerminalNode (keywords, key symbols, etc) becomes an Identifier
      case TerminalNode(_, other) => {
        Identifier(other.lexeme)
      }

      // 4. all kind UnaryOperatorNode becomes ApplyExpr with flattened parameter list
      case UnaryOperatorNode(kind, operator, operand) => {
        ApplyExpr(this.generateLIRO(operator), operand match {
          // 4.1. when there is a parameter list longer than length 1
          case SequenceNode('AParams, seq) => seq.map(this.generateLIRO(_))
          // 4.2. when there is only one parameter
          case node => Vector[liro.Object](this.generateLIRO(node))
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
          this.generateLIRO(operands._1) match {
            // Extract the operands
            case ApplyExpr(_, Vector(ListExpr(leftOperands))) => operands._2 match {
              // 5.1.1. right operand kind == currently processing kind and
              //        right operator == current operator, so do flattening work on it too
              // CASE: Both left and right operands are the same kind, so flatten both
              //       and append them to each other
              case BinaryOperatorNode(kindOperandRight, TerminalNode(kindOperatorRight,_), _)
                   if kindOperandRight == kind && kindOperatorRight == opKind => {
                // Generate operand2's LIR Object to extract the operands that need promoting
                this.generateLIRO(operands._2) match {
                  // Extract the operands and do the append
                  case ApplyExpr(_, Vector(ListExpr(rightOperands))) => leftOperands ++ rightOperands
                  // Error in extraction
                  case other => throw new Exception("ERROR") //TODO
                }
              }
              // 5.1.2. right operand kind =/= currently processing kind and
              //        right operator =/= current operator, then
              // CASE: Left Operand is same, right is not, so flatten only the left operand
              case operand2 => leftOperands :+ this.generateLIRO(operand2)
            }
            // Error in extraction
            case other => throw new Exception("ERROR") //TODO
          }
        }

        ApplyExpr(this.generateLIRO(operator), Vector(ListExpr(operands._1 match {
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
              this.generateLIRO(operands._2) match {
                // Extract the operands and do the flatten
                case ApplyExpr(_, Vector(ListExpr(rightOperands))) => this.generateLIRO(operand1) +: rightOperands
                // Error in extraction
                case other => throw new Exception("ERROR") //TODO
              }
            }
            // 5.2.2. right operand kind =/= currently processing kind and
            //        right operator =/= current operator, then
            // CASE: Both operands are not the same kind, so just make a length-2 parameter list
            //       with the left and right operands
            case operand2 => Vector[liro.Object](
              this.generateLIRO(operand1),
              this.generateLIRO(operand2)
            )
          }
        })))
      }

      // 6. other kind BinaryOperatorNode becomes ApplyExpr with length-2
      //    parameter list
      case BinaryOperatorNode(_, operator, operands) => {
        ApplyExpr(
          this.generateLIRO(operator),
          Vector[liro.Object](
            this.generateLIRO(operands._1),
            this.generateLIRO(operands._2)
          )
        )
      }

      // 7. Assign kind AssignNode becomes an AssignmentExpr with a basic
      //    value assigned to it
      case AssignNode('Assign, TerminalNode(_, idToken), assigned) => {
        AssignmentExpr(Identifier(idToken.lexeme), this.generateLIRO(assigned))
      }

      // 8. Assign kind OperatorAssignNode becomes an AssignmentExpr with an
      //    operator assigned to it
//      case OperatorAssignNode('Assign, TerminalNode(_, idToken), params, assigned) => {
//        AssignmentExpr(
//          Identifier(idToken.lexeme),
//          this.getOperatorExprFromFParamsAndBodyNodes(params, assigned)
//        )
//      }

      // 9. Statements kind StatementsNode becomes a BlockExpr containing
      //    the sequence of LIRO to execute in order of definition.
      case StatementsNode('Statements, statements) => {
        Logger.info('TRNSLTR, f"Processing StatementsNode $statements...")
        val res = BlockExpr(statements.map(s => this.generateLIRO(s)))
        Logger.info('TRNSLTR, f"Generated BlockExpr ${res.toRepr()}")
        res
      }

    } 
  }



  def processDecimalToken(dt: DecimalToken): RationalNumber = {
    Logger.info('TRNSLTR, f"Processing DecimalToken $dt...")

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
        case None => (w.reverse.mkString, f.reverse.mkString)
        case Some('_') => parseFractionalPartRec(s.tail, w, f)
        case Some(x: Char) => parseFractionalPartRec(s.tail, w, x :: f)
      }
    }

    val res = parseWholePartRec(dt.lexeme.toList) match {
      case (w, f) => {
        val numerator = BigInt(w ++ f)
        val denominator = BigInt(10).pow((f).length)
        RationalNumber(numerator, denominator)
      }
    }

    Logger.info('TRNSLTR, f"Generated RationalNumber ${res.toRepr()}")
    res
  }

  def processIntegerToken(it: IntegerToken): RationalNumber = {
    Logger.info('TRNSLTR, f"Processing IntegerToken $it...")
    val res = RationalNumber(BigInt(it.lexeme.filter(_ != '_')))
    Logger.info('TRNSLTR, f"Generated RationalNumber ${res.toRepr()}")
    res
  }

//  def getOperatorExprFromFParamsAndBodyNodes(
//    params: ParseNodeLike,
//    body: ParseNodeLike
//  ): OperatorExpr = {
//    params match {
//      case SequenceNode('FParams, )
//    }
//  }

}
