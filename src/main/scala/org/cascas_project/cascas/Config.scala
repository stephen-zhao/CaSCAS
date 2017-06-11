//=============================================================================
// Config.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

import scala.collection.mutable.{Map => MMap}
import scala.io.Source
import scala.util.matching._

import org.cascas_project.cascas.Util._

//=============================================================================
// Config object
//
// In charge of reading config files and providing configuration to the other
// components of CaSCAS.
//
object Config {
  
  type Section = String
  type Contents = MMap[Key, Value]
  type Key = String
  type Value = String
  type Ini = MMap[Section, Contents]
  
  private val sectionR: UnanchoredRegex = raw"""\[(.+)\]""".r.unanchored
  private val kvpR: UnanchoredRegex = raw"""(\w+)=(\w+)""".r.unanchored
  private val IntR: Regex = raw"""(\d+)""".r
  private val floatR: Regex = raw"""(\d+\.\d+)""".r

  private val configFile = sys.env("CASCAS_CONFIG_INI")

  private def createConfigFromIniFile(file: String): Ini = {
    var config: Ini = MMap[Section, Contents]()
    var currentSection: Section = ""
    for ((line, i) <- Source.fromFile(file).getLines.zipWithIndex) {
      line match {
        case sectionR(s) => {
          config += (s -> MMap[Key, Value]())
          currentSection = s
        }
        case kvpR(k, v) => {
          config(currentSection) += (k -> v)
        }
        case _ => {
          val err = f"""Bad syntax in config file on line $i: "$line""""
          Logger.error('CONFIG, err)
          throw new Exception(err)
        }
      }
    }
    config
  }

  def apply(section: Section): Contents = {
    config.getOrElse(section, MMap[Key, Value]())
  }

  def apply(section: Section, key: Key): Value = {
    config.get(section) match {
      case None => {
        val err = f"""Section "$section" does not exist in "$configFile""""
        Logger.error('CONFIG, err)
        throw new Exception(err)
      }
      case Some(s) => s.get(key) match {
        case None => {
          val err = f"""Key "$key" does not exist in section "$section" of "$configFile""""
          Logger.error('CONFIG, err)
          throw new Exception(err)
        }
        case Some(v) => v
      }
    }
  }

  val config: Ini = createConfigFromIniFile(configFile)

}
