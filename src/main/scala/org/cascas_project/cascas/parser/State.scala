package org.cascas_project.cascas.parser

import org.cascas_project.cascas.parser.CFG
import org.cascas_project.cascas.parser.Item

class State(
  val id: Int,
  val name: String,
  val transition: Symbol
) {

  var items: Set[Item] = Set[Item]()
  var childStates: Map[Symbol, State] = Map[Symbol, State]();

  def generateChildStatesFromItems(): Unit = {
    var visitedSymbols = Set[Symbol]()
    for (item <- this.items) {
      item.nextSymbol match {
        case Some(nextSymbol) => {
          if (!(visitedSymbols contains nextSymbol)) {
            makeNewChildState(nextSymbol)
            visitedSymbols += nextSymbol
          }
          this.childStates(nextSymbol).addItemAndPopulate(item.advanceMarker)
        }
        case None =>
      }
    }
  }

  def makeNewChildState(transition: Symbol): Unit = {
    val id = State.getUniqueStateId
    this.childStates += (transition -> new State(id, f"State_$id%4d", transition))
  }

  def addItemAndPopulate(item: Item): Unit = {
    this.items += item
    item.nextSymbol match {
      case Some(nextSymbol) if (CFG.nonterminals contains nextSymbol) => {
        val autoAddItems = getItemsWhereLHSIs(nextSymbol)
        for (autoItem <- autoAddItems) {
          if (!(this.items contains autoItem)) {
            addItemAndPopulate(autoItem)
          }
        }
      }
      case _ =>
    }
  }

  def isLastItemState(): Boolean = {
    this.items.head.isAtEnd
  }

  def recursivelyGenerateChildStates(): Unit = {
    this.generateChildStatesFromItems
    this.childStates.foreach(kvp => kvp._2.recursivelyGenerateChildStates)
  }

  def getItemsWhereLHSIs(nextSymbol: Symbol): Set[Item] = {
    CFG.rules.collect{ case (nextSymbol, v: Vector[Symbol]) => Item(nextSymbol, v, 0) }
  }

}

object State {
  
  var stateIdCounter = 0

  def getUniqueStateId(): Int = {
    stateIdCounter += 1
    stateIdCounter
  }

  def makeInitialState(): State = {
    val id = State.getUniqueStateId
    var res = new State(id, f"State_$id%4d", CFG.start)
    res.items ++= CFG.getStartingItems
    res
  }

  def makeStateGraph(): State = {
    var res = State.makeInitialState
    res.recursivelyGenerateChildStates
    res
  }

}
