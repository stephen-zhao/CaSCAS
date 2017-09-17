//=============================================================================
// parser/State.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

//=============================================================================

import org.cascas_project.cascas.Logger
import scala.collection.mutable.{Map => MMap}

//=============================================================================
// The State class represents a state in the parser's finite state machine.
//
// A state contains a set of items, to keep track of the potential positions at
// which the currently parsing sentence may be at, and a mapping from symbols
// to other states, to tell the parser which state to move to depending on the
// symbol read in from the currently parsing sentence.
//
class State(
  val id:         Int,
  val name:       String,
  val transition: Symbol
) {

  var items: Set[Item] = Set[Item]()
  var childStates: MMap[Symbol, State] = MMap[Symbol, State]()

  override def toString(): String = {
    f"${this.transition} -> ${this.name}\n" +
      "Items:\n" +
      f"  ${items.mkString("\n  ")}\n" +
      "Children:\n" +
      f"""  ${childStates.map(
        kvp => f"${kvp._1} -> ${kvp._2.name}"
      ).mkString("\n  ")}"""
  }

  def generateChildStatesFromItems(): Unit = {
    Logger.info('LRMG, f"Start Generating child states for ${this.name}")
    var visitedSymbols = Set[Symbol]()
    for (item <- this.items) {
      item.nextSymbol match {
        case Some(nextSymbol) => {
          if (!(visitedSymbols contains nextSymbol)) {
            Logger.verbose('LRMG, f"  Generating child state for $item")
            Logger.verbose('LRMG, f"    with transition $nextSymbol")
            this.childStates += (nextSymbol -> State.create(nextSymbol))
            visitedSymbols += nextSymbol
            Logger.verbose('LRMG, f"    and name #### ${this.childStates(nextSymbol).name}")
          }
          this.childStates(nextSymbol).addItemAndPopulate(item.advanceMarker)
        }
        case None =>
      }
    }
    Logger.verbose('LRMG, f"Finished Generating child states for ${this.name}")
  }

  def addItemAndPopulate(item: Item): Unit = {
    Logger.verbose('LRMG, f"    Adding item $item to state ${this.name}")
    this.items += item
    item.nextSymbol match {
      case Some(nextSymbol) if (CFG.nonterminals contains nextSymbol) => {
        val autoAddItems = CFG.rules.collect { 
          case (transition, v: Vector[Symbol]) if (transition == nextSymbol) => {
            new Item(nextSymbol, v, 0)
          }
        }
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
      this.childStates += (
        transition -> lrmg.checkReduceRegisterReturnState(state)
      )
    }
  }

  def isLastItemState(): Boolean = {
    this.items.exists(_.isAtEnd)
  }

  def getItemsWhereLHSIs(nextSymbol: Symbol): Set[Item] = {
    CFG.rules.collect{
      case (nextSymbol, v: Vector[Symbol]) => new Item(nextSymbol, v, 0)
    }
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
    state1.items == state2.items
  }

}
