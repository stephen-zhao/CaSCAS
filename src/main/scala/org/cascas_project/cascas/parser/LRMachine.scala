//=============================================================================
// LRMachine.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import org.cascas_project.cascas.Logger
import org.cascas_project.cascas.tokens.Token

case class LRMachine(
  val start: State
) {

  // Stack to hold the path of states being traversed
  // ** A state is a step in the process of collecting the terminals/nonterminals
  //    required to make a nonterminal node i.e. perform a derivation step.
  //    States are added to this stack when tokens from input or from the deriv
  //    stack match the items that the state contains. Once a derivation rule has
  //    been satisfied, i.e. states containing each of the items along the way
  //    have been added to the stack, all of these states are popped from the
  //    stack and a derivation is produced.
  var stateStack: List[State] = List[State](start)

  // the current state of the derivation
  def currentState(): State = {
    this.stateStack.head
  }

  // Stack to hold the current derivation
  var derivStack: List[ParseNode] = List[ParseNode]()

  // list of tokens that have been read in the current derivation instance
  var readTokens: List[Token] = List[Token]()

  // Resets the LRMachine's internal state, readying for a new derivation instance
  def resetMachineState(): Unit = {
    this.stateStack = List[State](start)
    this.derivStack = List[ParseNode]()
    this.readTokens = List[Token]()
  }

  // The public function to perform a rightmost derivation
  def rightmostDerive(
    tokens: Seq[Token],
    isNewDerivation: Boolean = true
  ): ParseNode = {

    Logger.info('LRM,  "Starting LR Rightmost Derivation")
    Logger.info('LRM, f"  on tokens:\n${tokens}")

    if (isNewDerivation) {
      Logger.info('LRM, "New derivation, so resetting LRMachine state.")
      this.resetMachineState
    }

    // Make tokens mutable
    var mtokens: Seq[Token] = tokens

    // While there are still states 
    while (!this.stateStack.isEmpty) {

      Logger.verbose('LRM, "Checking tokens for input")

      mtokens.headOption match {
        case None => {

          Logger.verbose('LRM, f"No more tokens.")

          this.whenTokenInputBad

        }
        case Some(input) => {

          Logger.verbose('LRM, f"Working on next token: $input")

          this.currentState.childStates.get(input.symbol) match {
            case None => {

              Logger.verbose('LRM, f"No transition with token.")

              this.whenTokenInputBad

            }
            case Some(nextState) => {

              Logger.verbose('LRM, f"Working on transition:")
              Logger.verbose('LRM, f"${this.currentState.name} -> " + 
                                   f"${input.symbol} -> ${nextState.name}")

              Logger.info('LRM, f"LR operation: SHIFT")

              this.readTokens = mtokens.head :: this.readTokens
              mtokens         = mtokens.tail
              this.stateStack = nextState :: this.stateStack
              this.derivStack = new TerminalNode(Symbol(input.lexeme), input) :: this.derivStack

              Logger.verbose('LRM, f"Terminal Node generated")
              Logger.verbose('LRM, f"Done working on token $input")
            }
          }
        }
      }
    }

    Logger.info('LRM, f"LR Rightmost Derivation complete!")
    Logger.info('LRM, f"Generated parse tree:\n${this.derivStack.head}")

    this.derivStack.head
  }


  // When input by token is bad (e.g. EOF), the derivation will proceed to check 
  // the derivation stack for non-terminal nodes to process. Eventually, all the
  // terminal nodes will be processed by the fn "rightmostDerivation" and all of
  // the non-terminal nodes will be processed by this fn until only the start
  // non-terminal node remains.
  private def whenTokenInputBad(): Unit = {
    
    Logger.verbose('LRM, f"Checking deriv stack for input")
    Logger.verbose('LRM, f"Current Deriv stack:\n${this.derivStack}")

    this.derivStack.headOption match {
      case None => {

        val err = f"LRMachine is in invalid situation! Nothing on deriv stack."
        Logger.error('LRM, err)
        throw new Exception(err)

      }
      case Some(input) => {

        Logger.verbose('LRM, f"Working on next node: $input")

        val item: Item = this.currentState.items.collectFirst {
          case x if x.isAtEnd => x
        }.getOrElse({
          val err = "Not at state with ending item!"
          Logger.error('LRM, err + f" Current state is: ${this.currentState}")
          throw new Exception(err)
        })

        Logger.verbose('LRM, f"Using item: $item to reduce stacks.")
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

