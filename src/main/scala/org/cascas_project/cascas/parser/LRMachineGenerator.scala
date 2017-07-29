//=============================================================================
// LRMachineGenerator.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import org.cascas_project.cascas.Logger

//=============================================================================
// LRMachineGenerator class
//
// Generates an LR finite state machine for parsing
//
class LRMachineGenerator() {

  type StateArray = ArrayBuffer[State]
  type StateDirectory = MMap[Symbol, StateArray]

  private var states: StateDirectory = MMap[Symbol, StateArray]()

  def generate(): LRMachine = {
    Logger.info('LRMG, "Starting LR finite state machine generation.")
    val lrm = new LRMachine(this.createStateGraph)
    Logger.info('LRMG, "Finished LR finite state machine generation.")
    lrm
  }

  private def createStateGraph(): State = {
    Logger.info('LRMG, f"Creating LR State Graph from starting symbol ${CFG.start}")
    var initialState = State.create(CFG.start)
    CFG.getStartingRules.collect{
      case (lhs, rhs) => new Item(lhs, rhs)
    }.foreach(item => initialState.addItemAndPopulate(item))
    Logger.info('LRMG, f"Initial state: ${initialState.id} ${initialState.transition}")
    Logger.verbose('LRMG, f"Initial state pre-generation items: ${initialState.items.mkString("\n")}")
    //this.recursivelyGenerateChildStates(initialState)
    this.iterativelyGenerateChildStates(initialState)
    initialState
  }

  private def recursivelyGenerateChildStates(state: State): Unit = {
    var state_ = state
    Logger.info('LRMG, f"Recursively generating child states for ${state.name}")
    Logger.verbose('LRMG, f"    Transition in is ${state.transition}")
    state_.generateChildStatesFromItems
    this.reduceChildStates(state_)
    Logger.info('LRMG, f"Finished generating child states for ${state.name}")
    Logger.verbose('LRMG, state.toString)
    state_.childStates.foreach(kvp => this.recursivelyGenerateChildStates(kvp._2))
  }

  private def iterativelyGenerateChildStates(state: State): Unit = {
    Logger.info('LRMG, f"Iteratively generating child states for ${state.name}")
    Logger.verbose('LRMG, f"    Transition in is ${state.transition}")
    var stateQueue: Queue[State] = new Queue[State]()
    var touchedIds: Set[Int] = Set()
    stateQueue.enqueue(state)
    touchedIds += state.id
    while (!stateQueue.isEmpty) {
      var state = stateQueue.dequeue
      state.generateChildStatesFromItems
      this.reduceChildStates(state)
      Logger.info('LRMG, f"Finished generating child states for ${state.name}")
      Logger.verbose('LRMG, state.toString)
      state.childStates.foreach(
        kvp => if (!(touchedIds contains kvp._2.id)) {
          touchedIds += kvp._2.id
          stateQueue.enqueue(kvp._2)
        }
      )
    }
  }

  private[parser] def checkReduceRegisterReturnState(state: State): State = {
    this.states.get(state.transition) match {
      case Some(stateArray) => {
        stateArray.find(s => State.isContainSameItems(state, s)) match {
          case Some(foundState) => {
            foundState
          }
          case None => {
            this.states(state.transition) += state
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

  private def reduceChildStates(state: State): Unit = {
    for ((transition, child) <- state.childStates) {
      state.childStates += (transition -> this.checkReduceRegisterReturnState(child))
    }
  }

}
