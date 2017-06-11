//=============================================================================
// LRMachine.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import org.cascas_project.cascas.Logger
import org.cascas_project.cascas.tokens.EndOfLineToken
import org.cascas_project.cascas.tokens.Token

case class LRMachine(
  val start: State
) {

  def rightmostDerive(tokens: Seq[Token]): ParseNode = {
    Logger.info('LRM,  "Starting LR Rightmost Derivation")
    Logger.info('LRM, f"  on tokens:\n${tokens}")

    // Stack to hold the path of states being traversed
    // ** A state is a step in the process of collecting the terminals/nonterminals
    //    required to make a nonterminal node i.e. perform a derivation step.
    //    States are added to this stack when tokens from input or from the deriv
    //    stack match the items that the state contains. Once a derivation rule has
    //    been satisfied, i.e. states containing each of the items along the way
    //    have been added to the stack, all of these states are popped from the
    //    stack and a derivation is produced.
    var stateStack: List[State] = List[State](start)

    // Stack to hold the current derivation
    var derivStack: List[ParseNode] = List[ParseNode]()

    // Make tokens mutable
    var mtokens: Seq[Token] = tokens


    def whenTokenInputBad(): Unit = {
      
      Logger.verbose('LRM, f"Checking deriv stack for input")
      Logger.verbose('LRM, f"Current Deriv stack:\n${derivStack}")

      derivStack.headOption match {
        case None => {

          val err = f"LRMachine is in invalid situation! Nothing on deriv stack."
          Logger.error('LRM, err)
          throw new Exception(err)

        }
        case Some(input) => {

          Logger.verbose('LRM, f"Working on next node: $input")

          var currentState = stateStack.head

          val item: Item = currentState.items.collectFirst {
            case x if x.isAtEnd => x
          }.getOrElse({
            val err = "Not at ending item!"
            Logger.error('LRM, err)
            throw new Exception(err)
          })

          Logger.verbose('LRM, f"Using item: $item to reduce stacks.")
          Logger.info('LRM, "LR operation: REDUCE")

          var nodesToAdd: Vector[ParseNode] = Vector[ParseNode]()
          for (symbol <- item.rhs.reverse) {
            stateStack = stateStack.tail
            nodesToAdd = derivStack.head +: nodesToAdd
            derivStack = derivStack.tail
          }

          currentState = stateStack.head

          currentState.childStates.get(item.lhs) match {
            case None => {

              if (item.lhs != start.transition) {
                val err = f"LR Machine is in invalid situation! No state with required transition: ${item.lhs}"
                Logger.error('LRM, err)
                throw new Exception(err)
              }
              else {
                stateStack = stateStack.tail
                derivStack = new NonTerminalNode(start.transition, nodesToAdd) :: derivStack
              }

            }
            case Some(nextState) => {
              
              stateStack = nextState :: stateStack
              derivStack = new NonTerminalNode(item.lhs, nodesToAdd) :: derivStack

            }

            Logger.verbose('LRM, f"Non Terminal Node generated")
            Logger.verbose('LRM, f"Done backtracking for item $item")

          }
        }
      }
    }

    // While there are still states 
    while (!stateStack.isEmpty) {

      val currentState: State = stateStack.head

      Logger.verbose('LRM, "Checking tokens for input")

      mtokens.headOption match {
        case None => {

          Logger.verbose('LRM, f"No more tokens.")

          whenTokenInputBad

        }
        case Some(input) => {

          Logger.verbose('LRM, f"Working on next token: $input")

          currentState.childStates.get(input.symbol) match {
            case None => {

              Logger.verbose('LRM, f"No transition with token.")

              whenTokenInputBad

            }
            case Some(nextState) => {

              Logger.verbose('LRM, f"Working on transition:")
              Logger.verbose('LRM, f"${currentState.name} -> ${input.symbol} -> ${nextState.name}")

              Logger.info('LRM, f"LR operation: SHIFT")

              mtokens = mtokens.tail
              stateStack = nextState :: stateStack
              derivStack = new TerminalNode(Symbol(input.lexeme), input) :: derivStack

              Logger.verbose('LRM, f"Terminal Node generated")
              Logger.verbose('LRM, f"Done working on token $input")
            }
          }
        }
      }
    }

    Logger.info('LRM, f"LR Rightmost Derivation complete!")
    Logger.info('LRM, f"Generated parse tree:\n${derivStack.head}")

    derivStack.head
  }

}

