//=============================================================================
// LRMachine.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import org.cascas_project.cascas.tokens.Token

case class LRMachine(
  val start: State
) {

  def rightmostDerive(expr: Seq[Token]): ParseNode = {
    println("LRMachine rightmostDerive enter")

    var stateStack: List[State] = List[State](start)
    var derivStack: List[ParseNode] = List[ParseNode]()

    var expression: Seq[Token] = expr

    while (!expression.isEmpty) {
      
      println("expression not empty, while")

      val input: Token = expression.head
      val currentState: State = stateStack.head
      
      println(f"input=$input, currentState=(${currentState.id}, ${currentState.transition})")

      currentState.childStates.get(input.symbol) match {
        case Some(nextState) => {
          println(f"Transition from currentState ${currentState.id} with symbol ${input.symbol} exists!")
          println(f"nextState=(${nextState.id}, ${nextState.transition})")
          expression = expression.tail
          stateStack = nextState :: stateStack
          derivStack = new TerminalNode(Symbol(input.lexeme), input) :: derivStack
        }
        case None => {
          println(f"Transition from currentState ${currentState.id} with symbol ${input.symbol} does not exist!")
          if (currentState.isLastItemState) {
            var nodesToAdd: Vector[ParseNode] = Vector[ParseNode]()
            for (symbol <- currentState.items.head.rhs.reverse) {
              stateStack = stateStack.tail
              nodesToAdd = derivStack.head +: nodesToAdd
              derivStack = derivStack.tail
            }
            derivStack = new NonTerminalNode(currentState.items.head.lhs, nodesToAdd) :: derivStack
          }
          else {
            sys.error("ERROR: state error")
          }
        }
      }
    }
    derivStack.head
  }

}

