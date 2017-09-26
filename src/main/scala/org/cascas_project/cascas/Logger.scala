//=============================================================================
// Logger.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas

//=============================================================================

import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream
import java.io.PrintStream
import java.io.PrintWriter
import java.io.StringWriter
import java.text.SimpleDateFormat
import java.util.Calendar

//=============================================================================

object Logger {

  type Descriptor = Symbol
  type Severity = Symbol
  type Tag = Symbol

  private val severityMap: Map[Severity, (Int, String)] = 
    Map('ERROR    -> (0, "ERROR"),
        'WARNING  -> (1, "WARNING"),
        'INFO     -> (2, "INFO"),
        'VERBOSE  -> (3, "VERBOSE"),
        'CALLSTACK-> (4, "CALL"))

  private val tagMap: Map[Tag, String] = 
    Map(
      'ANALYZER -> "ANALYZER",
      'APP      -> "CASCAS",
      'CONFIG   -> "CONFIG",
      'LEXER    -> "LEXER",
      'LRM      -> "LRM",
      'LRMG     -> "LRMG",
      'PARSER   -> "PARSER",
      'REPL     -> "REPL",
      'UTIL     -> "UTIL",
      'TRNSLTR  -> "TRNSLTR",
      'LIRO     -> "LIRO",
      'LANG     -> "LANG"
    )

  class Logger {

    private var tagToMaxSeverity: Map[Tag, Severity] = Map[Tag, Severity]()

    private var outputs: Vector[PrintStream] = Vector()

    def isLoggingActiveFor(severity: Severity, tag: Tag): Boolean = {
      if (this.tagToMaxSeverity.isEmpty) {
        tag match {
          case 'APP => severityMap(severity)._1 <= severityMap('INFO)._1
          case _    => severityMap(severity)._1 <= severityMap('WARNING)._1
        }
      }
      else {
        this.tagToMaxSeverity.get(tag) match {
          case None => {
            severityMap(severity)._1 <= severityMap('WARNING)._1
          }
          case Some(maxSeverity) => {
            severityMap(severity)._1 <= severityMap(maxSeverity)._1
          }
        }
      }
    }

    def setTagMaxSeverity(tag: Tag, severity: Severity): Unit = {
      require(tagMap contains tag)
      this.tagToMaxSeverity += (tag -> severity)
    }

    def write(severity: Severity, tag: Tag, msg: String): Unit = {
      require(severityMap contains severity)
      require(tagMap contains tag)
      if (this.isLoggingActiveFor(severity, tag)) {
        val now = Calendar.getInstance.getTime
        val time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(now)
        this.outputs.foreach(_.println(f"[$time][${severityMap(severity)._2}][${tagMap(tag)}] $msg"))
      }
    }

    def addOutput(output: PrintStream): Unit = {
      this.outputs = this.outputs :+ output
    }

    def stop(output: PrintStream): Unit = {
      this.outputs.foreach(_.flush())
      this.outputs.foreach(_.close())
    }
  }

  private val defaultLogFile: File =
    new File(sys.env("CASCAS_PROJECT_DIR") + "/log/cascas_" + new SimpleDateFormat("yyyyMMdd_HHmmss").format(App.appStartTime) + ".log")

  private def createDefaultInstance(): Logger = {
    var logger = new Logger()
    Config("Logger-Tags").foreach((kvp) => logger.setTagMaxSeverity(Symbol(kvp._1), Symbol(kvp._2)))
    if (sys.env("CASCAS_ENV") == "develop") {
      logger.addOutput(new PrintStream(new FileOutputStream(defaultLogFile)))
      logger.addOutput(Console.err)
    }
    else {
      logger.addOutput(Console.err)
    }
    logger
  }

  private val defaultLogger = createDefaultInstance()
 
  private def getStackTraceAsString(t: Throwable): String = {
    val sw = new StringWriter
    t.printStackTrace(new PrintWriter(sw))
    sw.toString
  }

  def write(severity: Severity, tag: Tag, msg: String): Unit = {
    defaultLogger.write(severity, tag, msg)
  }

  def error(tag: Tag, msg: String): Unit = {
    defaultLogger.write('ERROR, tag, msg)
  }

  def warning(tag: Tag, msg: String): Unit = {
    defaultLogger.write('WARNING, tag, msg)
  }

  def info(tag: Tag, msg: String): Unit = {
    defaultLogger.write('INFO, tag, msg)
  }

  def verbose(tag: Tag, msg: String): Unit = {
    defaultLogger.write('VERBOSE, tag, msg)
  }

  def callstack(tag: Tag, msg: String): Unit = {
    defaultLogger.write('CALLSTACK, tag, msg)
  }

  def exception(severity: Severity, tag: Tag, t: Throwable): Unit = {
    defaultLogger.write(severity, tag, getStackTraceAsString(t))
  }

  def exception(tag: Tag, t: Throwable): Unit = {
    defaultLogger.write('ERROR, tag, getStackTraceAsString(t))
  }

  def isLoggingActiveFor(severity: Severity, tag: Tag): Boolean = {
    defaultLogger.isLoggingActiveFor(severity, tag)
  }
}
