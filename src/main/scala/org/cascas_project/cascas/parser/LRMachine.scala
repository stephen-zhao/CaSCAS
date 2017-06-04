package org.cascas_project.cascas.parser

import org.cascas_project.cascas.parser.CFG
import org.cascas_project.cascas.parser.ParseNode
import org.cascas_project.cascas.parser.State
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
          expression = expression.tail
          stateStack = nextState :: stateStack
          derivStack = new TerminalNode(Symbol(input.lexeme), input) :: derivStack
        }
        case None => {
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

