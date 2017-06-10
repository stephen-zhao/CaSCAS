//=============================================================================
// State.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import org.cascas_project.cascas.Logger

class State(
  val id: Int,
  val name: String,
  val transition: Symbol
) {

  var items: Set[Item] = Set[Item]()
  var childStates: Map[Symbol, State] = Map[Symbol, State]()

  def generateChildStatesFromItems(): Unit = {
    Logger.info('LRMG, f"Start Generating child states for ${this.name}")
    var visitedSymbols = Set[Symbol]()
    for (item <- this.items) {
      item.nextSymbol match {
        case Some(nextSymbol) => {
          if (!(visitedSymbols contains nextSymbol)) {
            Logger.info('LRMG, f"  Generating child state for $item")
            Logger.info('LRMG, f"    with transition $nextSymbol")
            this.childStates += (nextSymbol -> State.create(nextSymbol))
            visitedSymbols += nextSymbol
          }
          this.childStates(nextSymbol).addItemAndPopulate(item.advanceMarker)
        }
        case None =>
      }
    }
    Logger.info('LRMG, f"Finished Generateing child states for ${this.name}")
    Logger.info('LRMG, f"${this.childStates}")
  }

  def addItemAndPopulate(item: Item): Unit = {
    this.items += item
    item.nextSymbol match {
      case Some(nextSymbol) if (CFG.nonterminals contains nextSymbol) => {
        //val autoAddItems = this.getItemsWhereLHSIs(nextSymbol)
        val autoAddItems = CFG.rules.collect{ case (nextSymbol, v: Vector[Symbol]) => new Item(nextSymbol, v, 0) }
        for (autoItem <- autoAddItems) {
          if (!(this.items contains autoItem)) {
           this.addItemAndPopulate(autoItem)
          }
        }
      }
      case _ =>
    }
  }

  def reduceChildStates(lrmg: LRMachineGenerator): Unit = {
    for ((transition, state) <- this.childStates) {
      this.childStates += (transition -> lrmg.checkReduceRegisterReturnState(state))
    }
  }

  def isLastItemState(): Boolean = {
    this.items.head.isAtEnd
  }

  def recursivelyGenerateChildStates(lrmg: LRMachineGenerator): Unit = {
    Logger.info('LRMG, f"Recursively generating child states for ${this.name}")
    Logger.info('LRMG, f"    Transition in is ${this.transition}")
    this.generateChildStatesFromItems
    this.reduceChildStates(lrmg)
    this.childStates.foreach(kvp => kvp._2.recursivelyGenerateChildStates(lrmg))
  }

  def getItemsWhereLHSIs(nextSymbol: Symbol): Set[Item] = {
    CFG.rules.collect{ case (nextSymbol, v: Vector[Symbol]) => new Item(nextSymbol, v, 0) }
  }

}

object State {
  
  var stateIdCounter = 0

  def getUniqueStateId(): Int = {
    stateIdCounter += 1
    stateIdCounter
  }

  def create(transition: Symbol): State = {
    val id = State.getUniqueStateId
    new State(id, f"State_$id%04d", transition)
  }
  
  def isContainSameItems(state1: State, state2: State): Boolean = {
    false
  }

}
