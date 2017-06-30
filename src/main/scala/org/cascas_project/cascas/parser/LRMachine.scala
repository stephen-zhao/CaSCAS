//=============================================================================
// LRMachine.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import scala.annotation._
import org.cascas_project.cascas.Logger
import org.cascas_project.cascas.tokens.Token


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
  val start: State
) {

  // Holds the status of the current derivation if one is in progress, or of
  // the last derivation if there isn't one in progress.
  private var derivationStatus: LRMachine.DerivationStatus = LRMachine.NotStarted
  def getDerivationStatus() = { this.derivationStatus }

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
  private def currentState(): State = {
    this.stateStack.head
  }

  // Stack to hold the current derivation
  private var derivStack: List[ParseNode] = List[ParseNode]()

  // list of tokens that have been read in the current derivation instance
  private var readTokens: List[Token] = List[Token]()

  // Resets the LRMachine's internal state, readying for a new derivation instance
  private def resetMachineState(): Unit = {
    this.stateStack = List[State](start)
    this.derivStack = List[ParseNode]()
    this.readTokens = List[Token]()
    this.derivationStatus = LRMachine.NotStarted
  }

  // The public function to perform a rightmost derivation, given new tokens to
  // work with and whether to start from scratch or to continue with the previous
  // derivation.
  def rightmostDerive(
    tokens: Seq[Token],
    isNewDerivation: Boolean = true
  ): Option[ParseNode] = {

    Logger.info('LRM,  "Starting LR Rightmost Derivation")
    Logger.info('LRM, f"  on tokens:\n${tokens}")

    if (isNewDerivation) {
      Logger.info('LRM, "New derivation, so resetting LRMachine state.")
      this.resetMachineState
    }

    this.derivationStatus = LRMachine.Deriving

    val result = this.workOnTokenInput(tokens)

    result match {
      case None => {

        Logger.warning('LRM, f"LR Rightmost Derivation hit a problem!")

        None

      }
      case Some(tree) => {
        
        Logger.info('LRM, f"LR Rightmost Derivation complete!")
        Logger.info('LRM, f"Generated parse tree:\n${tree}")

        result

      }
    }
  }


  @tailrec
  private def workOnTokenInput(
    tokens: Seq[Token]
  ): Option[ParseNode] = this.derivationStatus match {
    case LRMachine.Deriving => {
      if (!this.stateStack.isEmpty) {

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
                this.derivStack = new TerminalNode(Symbol(input.lexeme), input) :: this.derivStack

                Logger.verbose('LRM, f"Terminal Node generated")
                Logger.verbose('LRM, f"Done working on token $input")

                this.workOnTokenInput(tokens.tail)
              
              }
            }
          }
        }
      }
      else {
        Logger.info('LRM, f"LR Rightmost Derivation complete!")
        Logger.info('LRM, f"Generated parse tree:\n${this.derivStack.head}")

        this.derivationStatus = LRMachine.Success

        Some(this.derivStack.head)
      }
    }
    case _ => None
  }


  private def workOnDerivStackInput(badTokenOption: Option[Token]): Unit = {
    
    Logger.verbose('LRM, f"Checking deriv stack for input")
    Logger.verbose('LRM, f"Current Deriv stack:\n${this.derivStack}")

    this.derivStack.headOption match {
      case None => {

        val err = f"LRMachine is in invalid situation! Nothing on deriv stack."
        Logger.error('LRM, err)
        
        this.derivationStatus = LRMachine.Failure

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

              }
              case None => {

                Logger.info('LRM, err + f"Current state is: ${this.currentState}")

                this.derivationStatus = LRMachine.NeedsTokens

              }
            }
          }
          case Some(item) => {

            Logger.verbose('LRM, f"Using ending item: $item to reduce stacks.")
            Logger.info('LRM, "LR operation: REDUCE")

            var nodesToAdd: Vector[ParseNode] = Vector[ParseNode]()

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
                  this.derivStack = new NonTerminalNode(this.start.transition, nodesToAdd) ::
                                    this.derivStack
                }

              }
              case Some(nextState) => {

                Logger.verbose('LRM, "And finally shift with transition:")
                Logger.verbose('LRM, f"${this.currentState.name} -> " +
                                     f"${item.lhs} -> ${nextState.name}")
                
                this.stateStack = nextState :: this.stateStack
                this.derivStack = new NonTerminalNode(item.lhs, nodesToAdd) :: this.derivStack

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

