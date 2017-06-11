//=============================================================================
// LRMachineGenerator.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import scala.collection.mutable.ArrayBuffer
import org.cascas_project.cascas.Logger

//=============================================================================
// LRMachineGenerator class
//
// Generates an LR finite state machine 
class LRMachineGenerator() {

  type StateArray = ArrayBuffer[State]
  type StateDirectory = Map[Symbol, StateArray]

  private var states: StateDirectory = Map[Symbol, StateArray]()

  def generate(): LRMachine = {
    Logger.info('LRMG, "Starting LR finite state machine generation.")
    val lrm = new LRMachine(this.createStateGraph)
    Logger.info('LRMG, "Finished LR finite state machine generation.")
    lrm
  }

  private def createStateGraph(): State = {
    Logger.info('LRMG, "Creating LR State Graph")
    var initialState = State.create(CFG.start)
    CFG.getStartingRules.collect{
      case (lhs, rhs) => new Item(lhs, rhs)
    }.foreach(item => initialState.addItemAndPopulate(item))
    Logger.verbose('LRMG, f"Initial state: ${initialState.id} ${initialState.transition}")
    Logger.verbose('LRMG, f"Initial state pre-generation items: ${initialState.items}")
    this.recursivelyGenerateChildStates(initialState)
    initialState
  }

  private def recursivelyGenerateChildStates(state: State): Unit = {
    var state_ = state
    Logger.verbose('LRMG, f"Recursively generating child states for ${state.name}")
    Logger.verbose('LRMG, f"    Transition in is ${state.transition}")
    state.generateChildStatesFromItems
    state.reduceChildStates(this)
    state.childStates.foreach(kvp => this.recursivelyGenerateChildStates(kvp._2))
  }

  private[parser] def checkReduceRegisterReturnState(state: State): State = {
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
