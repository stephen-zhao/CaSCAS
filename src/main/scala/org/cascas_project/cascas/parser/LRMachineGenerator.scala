package org.cascas_project.cascas.parser

import org.cascas_project.cascas.parser.LRMachine
import org.cascas_project.cascas.parser.State

class LRMachineGenerator() {

  def generate(): LRMachine = {
    new LRMachine(State.makeStateGraph)
  }

}
