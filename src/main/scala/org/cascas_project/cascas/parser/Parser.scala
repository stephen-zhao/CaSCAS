//=============================================================================
// Parser.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import scala.annotation.tailrec
import org.cascas_project.cascas.Interpreter
import org.cascas_project.cascas.Lexer
import org.cascas_project.cascas.Logger
import org.cascas_project.cascas.tokens._
import org.cascas_project.cascas.parser._


object Parser {

  // Parser Parse Status Enum
  sealed trait ParseStatus { def name: String }
  case object Success extends ParseStatus { val name = "Success" }
  case object Failure extends ParseStatus { val name = "Failure" }

}


class Parser {

  private val lrm: LRMachine = new LRMachineGenerator().generate.attachParser(this)

  private val lexer: Lexer = new Lexer

  private var numParseCalls: Int = 0

  protected val isIgnoredTokenDefault: Boolean = false

  protected val isIgnoredTokenFn: PartialFunction[Token, Boolean] = {
    case WhitespaceToken(_, _) => true
    case CommentToken(_, _)    => true
  }

  protected def displayPrompt(): Unit = {}

  protected def displayContinuedPrompt(): Unit = {}

  protected def scanTokens(
    lineNum: Int,
    isContinuedScan: Boolean
  ): Vector[Token] = this.lexer.scanProgram(
    displayPrompt=this.displayPrompt,
    displayContPrompt=this.displayContinuedPrompt,
    useContPrompt=isContinuedScan
  )

  //TODO: get rid of all the asInstanceOf calls. It's bad programming.
  def generateNode(
    lhs: Symbol,
    rhs: Vector[ParseNodeLike]
  ): ParseNodeLike = {
    (lhs, rhs.length) match {

      // This is Statements within some brackets, just use the Statements node
      case ('Base, 3) => rhs(1)

      // Treat all function applications as a unary operator on a single list
      // of parameters.
      case ('Apply, 4) => UnaryOperatorNode(lhs, rhs(0), rhs(2))

      // Treat all of these nodes as binary operators, even if they are chained
      case ('BoolExpr | 
            'BoolOrend |
            'Relation | 
            'MathExpr |
            'Term |
            'Exponent, 3) => BinaryOperatorNode(lhs, rhs(1), (rhs(0), rhs(2)))
      
      // This is a boolean NOT
      case ('BoolAndend, 2) => UnaryOperatorNode(lhs, rhs(0), rhs(1))
      
      // This is either an algebraic negation or a factorial
      case ('Factor, 2) => if (rhs(0).kind == 'Factor) {
                             UnaryOperatorNode(lhs, rhs(1), rhs(0))
                           }
                           else {
                             UnaryOperatorNode(lhs, rhs(0), rhs(1))
                           }

      // Assignment to a name, without a parameter list (although the
      // expression being assigned may be a lambda, in which case the parameter
      // list will be dealt with later.
      case ('Assign, 4) => AssignNode(lhs, rhs(1), rhs(3))
      
      // Assignment to a name with a parameter list, i.e. an operator
      // definition.
      case ('Assign, 7) => OperatorAssignNode(lhs, rhs(1),
        rhs(3).asInstanceOf[SequenceNode],
        rhs(6))

      // Reassignment to a name (no parameter list allowed)
      case ('ReAssign, 3) => ReAssignNode(lhs, rhs(0), rhs(2))

      // A sequence of statements will need a different form of analysis later,
      // so it should be parsed differently from other sequences. Flatten the
      // statements tree here, during the parse.
      case ('Statements, 3) => StatementsNode(lhs,
        rhs(0).asInstanceOf[StatementsNode].statements :+ rhs(2)
      )

      // Due to the above type assumption, a single RHS node should still be
      // properly wrapped within a statements node
      case ('Statements, 1) => StatementsNode(lhs,
        Vector[ParseNodeLike](rhs(0))
      )

      // Lambdas get their own special node
      case ('Lambda, 7) => LambdaNode(lhs, rhs(2), rhs(5))

      // For all other sequenced types (comma delimited), flatten the sequences
      // right here during the parse.
      case ('FParams |
            'AParams |
            'SetIn |
            'ListIn, 3) => SequenceNode(lhs,
              rhs(0).asInstanceOf[SequenceNode].sequence :+ rhs(2)
            )

      // Due to the above assumption, a single RHS node must be properly
      // wrapped within a sequence node
      case ('FParams |
            'AParams |
            'SetIn |
            'ListIn, 1) => SequenceNode(lhs,
              Vector[ParseNodeLike](rhs(0))
            )

      // For non-empty collections, just use the sequence node directly
      case ('Set |
            'List, 3) => rhs(1)

      // For empty collections, make an empty sequence node
      case ('Set |
            'List, 2) => SequenceNode(lhs, Vector[ParseNodeLike]())

      // While loops get a special node type
      case ('WhileControl, 7) => WhileNode(lhs, rhs(2), rhs(5))
      
      // For loops get a special node type
      case ('ForControl, 9) => ForNode(lhs, rhs(1), rhs(4), rhs(7))

      // If/Elsif get a special node type. Elsifs just become nested if
      // statements.
      case ('IfControl |
            'ElControl, 8) => IfNode(lhs, rhs(2), rhs(5), rhs(7))

      // Else is the same as a statements node, so just use the node without
      // any special wrapper
      case ('ElControl, 4) => rhs(2)

      //case (_, 1) => WrapperNode(rhs(0))(lhs)
      // All other cases where RHS has only node, just use it instead of 
      // wrapping it up in anything special.
      case (_, 1) => rhs(0)
    }
  }

  def createTerminalNode(
    token: Token
  ): TerminalNode = {
    TerminalNode(token.symbol, token)
  }

  def parse(): ParseNodeLike = {
    val result = this.parseUntilValidProgram()
    this.numParseCalls = this.numParseCalls + 1
    result match {
      case None       => throw new Exception("Bad parse!")
      case Some(tree) => tree
    }
  }

  def parseOption(): Option[ParseNodeLike] = {
    val result = this.parseUntilValidProgram()
    this.numParseCalls = this.numParseCalls + 1
    result
  }

  @tailrec
  private def parseUntilValidProgram(
    isFirst: Boolean = true
  ): Option[ParseNodeLike] = {

    val tokens = this.scanTokens(0, !isFirst).filter(this.isNotIgnoredToken)

    if (tokens.isEmpty) {
      
      Logger.info('PARSER, "No tokens parsed.")
      
      None

    }
    else {

      this.lrm.rightmostDerive(tokens, isFirst) match {
        case None => {
          this.lrm.getDerivationStatus match {
            case LRMachine.Failure => {
              
              Logger.error('PARSER, "Tokens failed to parse.")
              None

            }
            case LRMachine.NeedsTokens => {

              Logger.info('PARSER, "Tokens are parsing. Need more to continue...")
              this.parseUntilValidProgram(false)

            }
            case others => {
              val err = "Invalid LRMachine status for no parse tree returned." +
                        f" Status=${others.name}"
              Logger.error('PARSER, err)
              throw new Exception(err)
            }
          }
        }
        case Some(tree) => {

          Logger.info('PARSER, "Tokens successfully parsed.")
          Some(tree)

        }
      }

    }
  }

  private def isNotIgnoredToken(token: Token): Boolean = {
    !( if (this.isIgnoredTokenFn.isDefinedAt(token)) {
        this.isIgnoredTokenFn(token)
      }
      else {
        this.isIgnoredTokenDefault
      }
    )
  }

}


class InteractiveParser(
  interpreter: Interpreter
) extends Parser {

  protected override def displayPrompt(): Unit = {
    this.interpreter.displayInputPrompt
  }

  protected override def displayContinuedPrompt(): Unit = {
    this.interpreter.displayContinuedInputPrompt
  }

}
