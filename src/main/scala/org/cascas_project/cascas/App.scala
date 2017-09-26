//=============================================================================
// App.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

//=============================================================================

import java.util.Calendar

//=============================================================================
// Main Application Object
// 
object App {

  val appStartTime = Calendar.getInstance.getTime
  
  //===========================================================================
  // Main function
  //
  def main(args : Array[String]): Unit = {


    Logger.info('APP, f"""running "CaSCAS${if (args.length > 0) (" " /: args)((x,y) => x + " " + y) else ""}"""")
    
    // Catch program level exceptions
    // Bad practice to use try blocks in Scala
    // This is temporary, but may be here to stay
    try {
      Logger.info('APP, "CaSCAS is starting up.")

      (new Interpreter).repl()

      Logger.info('APP, "CaSCAS has terminated successfully.")
    }
    catch {
      // Catch all exceptions
      case e: Throwable => {
        Logger.exception('APP, e)
        Logger.error('APP, "CaSCAS has terminated unexpectedly.")
      }
    }
  }

}
