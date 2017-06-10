//=============================================================================
// LRMachineGenerator.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import scala.collection.mutable.ArrayBuffer

class LRMachineGenerator() {

  type StateArray = ArrayBuffer[State]
  type StateDirectory = Map[Symbol, StateArray]

  var states: StateDirectory = Map[Symbol, StateArray]()

  def generate(): LRMachine = {
    new LRMachine(createStateGraph)
  }

  def createStateGraph(): State = {
    println("Creating LR State Graph")
    var initialState = State.create(CFG.start)
    CFG.getStartingRules.collect{
      case (lhs, rhs) => new Item(lhs, rhs)
    }.foreach(item => initialState.addItemAndPopulate(item))
    println(f"Initial state: ${initialState.id} ${initialState.transition}")
    println(f"Initial state pre-generation items: ${initialState.items}")
    initialState.recursivelyGenerateChildStates(this)
    initialState
  }

  def checkReduceRegisterReturnState(state: State): State = {
    this.states.get(state.transition) match {
      case Some(stateArray) => {
        stateArray.find(s => State.isContainSameItems(state, s)) match {
          case Some(foundState) => {
            foundState
          }
          case None => {
            stateArray += state
            state
          }
        }
      }
      case None => {
        this.states += (state.transition -> new ArrayBuffer[State]())
        this.states(state.transition) += state
        state
      }
    }
  }

}
