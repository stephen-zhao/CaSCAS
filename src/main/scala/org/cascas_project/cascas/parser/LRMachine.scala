//=============================================================================
// parser/LRMachine.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

//=============================================================================

import org.cascas_project.cascas.Logger
import org.cascas_project.cascas.parsetree.{ParseNodeLike, TerminalNode}
import org.cascas_project.cascas.token.Token
import scala.annotation.tailrec

//=============================================================================


object LRMachine {
  
  // LR Machine Derivation Status Enum
  sealed trait DerivationStatus { def name: String }
  case object NotStarted extends DerivationStatus { val name = "NotStarted" }
  case object Deriving extends DerivationStatus { val name = "Deriving" }
  case object NeedsTokens extends DerivationStatus { val name = "NeedsTokens" }
  case object Success extends DerivationStatus { val name = "Success" }
  case object Failure extends DerivationStatus { val name = "Failure" }

}

//=============================================================================
// LRMachine class
//
// This class represents a Left-to-Right Rightmost Parsing Finite State
// Machine. It holds the machine's internal state and provides public methods
// to perform derivations on sequences of tokens that produce parse trees
//
case class LRMachine(
  start: State
) {

  // The parser that is using this LRMachine
  private var parserOption: Option[Parser] = None

  // Use this public function to attach a parser to this LRMachine
  def attachParser(parser: Parser): LRMachine = {
    this.parserOption = Some(parser)
    this
  }

  // Use this public function to detach the currently attached parser
  def detachParser(): LRMachine = {
    this.parserOption = None
    this
  }

  // Holds the status of the current derivation if one is in progress, or of
  // the last derivation if there isn't one in progress.
  private var derivationStatus: LRMachine.DerivationStatus = LRMachine.NotStarted
  def getDerivationStatus: LRMachine.DerivationStatus = { this.derivationStatus }

  // Stack to hold the path of states being traversed
  // ** A state is a step in the process of collecting the terminals/nonterminals
  //    required to make a nonterminal node i.e. perform a derivation step.
  //    States are added to this stack when tokens from input or from the deriv
  //    stack match the items that the state contains. Once a derivation rule has
  //    been satisfied, i.e. states containing each of the items along the way
  //    have been added to the stack, all of these states are popped from the
  //    stack and a derivation is produced.
  private var stateStack: List[State] = List[State](start)

  // the current state of the derivation
  private def currentState: State = {
    this.stateStack.head
  }

  // Stack to hold the current derivation
  private var derivStack: List[ParseNodeLike] = List[ParseNodeLike]()

  // list of tokens that have been read in the current derivation instance
  private var readTokens: List[Token] = List[Token]()

  // Resets the LRMachine's internal state, readying for a new derivation instance
  private def resetMachineState(): Unit = {
    this.stateStack = List[State](start)
    this.derivStack = List[ParseNodeLike]()
    this.readTokens = List[Token]()
    this.derivationStatus = LRMachine.NotStarted
  }

  // This private function creates a generic node. It delegates work to the
  // factory method in the parser
  private def createNode(
    lhs: Symbol,
    rhs: Vector[ParseNodeLike]
  ): ParseNodeLike = {
    this.parserOption match {
      case Some(parser) => parser.generateNode(lhs, rhs)
      case None => {
        val err = "No parser attached to LRMachine! Cannot create a nonterminal node!"
        Logger.error('LRM, err)
        throw new Exception(err)
      }
    }
  }

  // This private function creates a terminal node specifically. It delegates
  // work to the factory method in the parser
  private def createTerminalNode(lhs: Symbol, token: Token): TerminalNode = {
    this.parserOption match {
      case Some(parser) => parser.createTerminalNode(token)
      case None => {
        val err = "No parser attached to LRMachine! Cannot create a terminal node!"
        Logger.error('LRM, err)
        throw new Exception(err)
      }
    }
  }

  // The public function to perform a rightmost derivation, given new tokens to
  // work with and whether to start from scratch or to continue with the previous
  // derivation.
  def rightmostDerive(
    tokens: Seq[Token],
    isNewDerivation: Boolean = true
  ): Option[ParseNodeLike] = {

    Logger.info('LRM,  "Starting LR Derivation")
    Logger.info('LRM, f"  on tokens:\n${tokens}")

    if (isNewDerivation) {
      Logger.info('LRM, "New derivation, so resetting LRMachine state.")
      this.resetMachineState()
    }

    this.derivationStatus = LRMachine.Deriving

    val result = this.workOnTokenInput(tokens)

    result match {
      case None => {

        None

      }
      case Some(tree) => {
        
        Logger.info('LRM, f"LR Derivation complete!")
        Logger.info('LRM, f"Generated parse tree:\n${tree}")

        result

      }
    }
  }


  @tailrec
  private def workOnTokenInput(
    tokens: Seq[Token]
  ): Option[ParseNodeLike] = this.derivationStatus match {
    case LRMachine.Deriving => {
      if (this.stateStack.nonEmpty) {

        Logger.verbose('LRM, "Checking tokens for input")

        tokens.headOption match {
          case None => {

            Logger.verbose('LRM, f"No more tokens.")

            this.workOnDerivStackInput(None)
            this.workOnTokenInput(tokens)

          }
          case Some(input) => {

            Logger.verbose('LRM, f"Working on next token: $input")

            this.currentState.childStates.get(input.symbol) match {
              case None => {

                Logger.verbose('LRM, f"No transition with token.")

                this.workOnDerivStackInput(Some(input))
                this.workOnTokenInput(tokens)

              }
              case Some(nextState) => {

                Logger.verbose('LRM, f"Working on transition:")
                Logger.verbose('LRM, f"${this.currentState.name} -> " +
                                     f"${input.symbol} -> ${nextState.name}")

                Logger.info('LRM, f"LR operation: SHIFT")

                this.readTokens = tokens.head :: this.readTokens
                this.stateStack = nextState :: this.stateStack
                // Make the terminal node
                this.derivStack = this.createTerminalNode(
                  Symbol(input.lexeme), input
                ) :: this.derivStack

                Logger.verbose('LRM, f"Terminal Node generated")
                Logger.verbose('LRM, f"Done working on token $input")

                this.workOnTokenInput(tokens.tail)
              
              }
            }
          }
        }
      }
      else {
        if (tokens.isEmpty) {
          
          Logger.info('LRM, f"LR Derivation token input parsing is successful!")

          this.derivationStatus = LRMachine.Success
          
          Logger.info('LRM, f"derivationStatus=${this.derivationStatus}")

          Some(this.derivStack.head)
        
        }
        else {

          Logger.error('LRM, f"LR Derivation token input parsing not successful! Extra tokens: $tokens")

          this.derivationStatus = LRMachine.Failure

          Logger.error('LRM, f"derivationStatus=${this.derivationStatus}")
          
          None

        }
      }
    }
    case _ => None
  }


  private def workOnDerivStackInput(badTokenOption: Option[Token]): Unit = {
    
    Logger.verbose('LRM, f"Checking deriv stack for input")
    Logger.verbose('LRM, f"Current Deriv stack:\n${this.derivStack}")

    this.derivStack.headOption match {
      case None => {

        Logger.error('LRM, f"LRMachine is in invalid situation! Nothing on deriv stack.")
        
        this.derivationStatus = LRMachine.Failure

        Logger.info('LRM, f"derivationStatus=${this.derivationStatus}")
        Logger.error('LRM, f"LR Derivation hit a problem!")
      
      }
      case Some(input) => {

        Logger.verbose('LRM, f"Working on next node: $input")

        this.currentState.items.collectFirst {
          case x if x.isAtEnd => x
        } match {
          case None => {

            val err = "Not at state with ending item! "

            badTokenOption match {
              case Some(badToken) => {

                Logger.error('LRM, err + f"And next token is bad: $badToken. " +
                                   f"Current state is: ${this.currentState}")

                this.derivationStatus = LRMachine.Failure
        
                Logger.info('LRM, f"derivationStatus=${this.derivationStatus}")
                Logger.error('LRM, f"LR Derivation hit a problem!")

              }
              case None => {

                Logger.info('LRM, err + f"Current state is: ${this.currentState}")

                this.derivationStatus = LRMachine.NeedsTokens

                Logger.info('LRM, f"derivationStatus=${this.derivationStatus}")
                Logger.info('LRM, f"LR Derivation needs more tokens!")
              
              }
            }
          }
          case Some(item) => {

            Logger.verbose('LRM, f"Using ending item: $item to reduce stacks.")
            Logger.info('LRM, "LR operation: REDUCE")

            var nodesToAdd: Vector[ParseNodeLike] = Vector[ParseNodeLike]()

            for (symbol <- item.rhs.reverse) {
              Logger.verbose('LRM, "Reducing with transition:")
              Logger.verbose('LRM, f"${this.currentState.name} -> " +
                                   f"${symbol} -> ${this.stateStack.tail.head.name}")

              this.stateStack = this.stateStack.tail
              nodesToAdd      = this.derivStack.head +: nodesToAdd
              this.derivStack = this.derivStack.tail

            }

            this.currentState.childStates.get(item.lhs) match {
              case None => {

                if (item.lhs != this.start.transition) {
                  val err = f"LR Machine is in invalid situation!" +
                            f"No state with required transition: ${item.lhs}"
                  Logger.error('LRM, err)
                  throw new Exception(err)
                }
                else {
                  this.stateStack = this.stateStack.tail
                  this.derivStack = this.createNode(
                    this.start.transition, nodesToAdd
                  ) :: this.derivStack
                }

              }
              case Some(nextState) => {

                Logger.verbose('LRM, "And finally shift with transition:")
                Logger.verbose('LRM, f"${this.currentState.name} -> " +
                                     f"${item.lhs} -> ${nextState.name}")
                
                this.stateStack = nextState :: this.stateStack
                this.derivStack = this.createNode(
                  item.lhs, nodesToAdd
                ) :: this.derivStack

              }

              Logger.verbose('LRM, f"Non Terminal Node generated")
              Logger.verbose('LRM, f"Done backtracking for item $item")

            }
          }
        }
      }
    }
  }

}

