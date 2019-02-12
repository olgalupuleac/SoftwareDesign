package ru.spbau.lupuleac.cli

import scala.sys.process._

/**
  * Command in interpreter.
  */
trait Command {
  /*
   * Takes the arguments, executes the command and returns the output with the results.
   */
  def apply(arguments: Argument*): Output
}

/**
  * Returns all arguments, separated by space
  */
case class EchoCommand(output: Output) extends Command {
  override def apply(arguments: Argument*): Output = Output(arguments.map(x => x.asString).mkString(" "))
}

/**
  * Returns number of lines, words and bytes in each file provided
  * as an argument and total number of lines, words and bytes.
  */
case class WcCommand() extends Command {
  override def apply(arguments: Argument*): Output = {
    val list = arguments.map(x => (x.asFile.split("\n").length, x.asFile.split("\\s").length,
      x.asFile.getBytes().length, x.asString))
    val totalLines = list.foldLeft(0)((x: Int, y: (Int, Int, Int, String)) => x + y._1)
    val totalWords = list.foldLeft(0)((x: Int, y: (Int, Int, Int, String)) => x + y._2)
    val totalBytes = list.foldLeft(0)((x: Int, y: (Int, Int, Int, String)) => x + y._3)
    val res = list.map(x => List(x._1.toString, x._2.toString, x._3.toString, x._4).mkString(" ").trim)
    val total = List(totalLines, totalWords, totalBytes).map(x => x.toString).mkString(" ") + " total"
    Output((res :+ total).mkString("\n"))
  }
}

/**
  * Returns a file (or files) contents.
  */
case class CatCommand() extends Command {
  override def apply(arguments: Argument*): Output = {
    Output(arguments.map(x => x.asFile).mkString("\n"))
  }
}

/**
  * Returns the current working directory (and ignores) it's arguments.
  */
case class PwdCommand() extends Command {
  override def apply(arguments: Argument*): Output = Output(System.getProperty("user.dir"))
}

/**
  * Exits from the interpreter.
  */
case class ExitCommand() extends Command {
  override def apply(arguments: Argument*): Output = {
    System.exit(0)
    Output("")
  }
}

/**
  * Makes a system call and executes the command with its arguments.
  *
  * @param command is an unknown command to be executed
  */
case class ProcessCommand(command: String) extends Command {
  override def apply(arguments: Argument*): Output = {
    val args = arguments.map(x => x.asString).mkString(" ")
    val sysCommand = command + " " + args
    Output(Process(sysCommand) !!)
  }
}


import org.rogach.scallop._

case class GrepConf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val ignoreCase: ScallopOption[Boolean] = toggle(name = "ignore-case", default = Some(false), descrYes = "Ignore case distinctions in both the PATTERN and the input files.")
  val wordRegex: ScallopOption[Boolean] = toggle(name = "word-regexp", default = Some(false), descrYes = "Select only those lines containing matches that form whole words.\n" +
    "The test is that the matching substring must either be at the beginning of the line, or preceded by a non-word constituent character. Similarly, it must be either at the end of the line or followed by a non-word constituent character. Word-constituent characters are letters, digits, and the underscore.")
  val afterContext: ScallopOption[Int] = opt[Int](short = 'A', descr = "Print NUM lines of trailing context after matching lines.", validate = x => x >= 0, argName = "NUM")
  val pattern: ScallopOption[String] = trailArg[String](validate = x => try {
    x.r
    true
  } catch {
    case _: Exception => false
  })
  val files: ScallopOption[List[String]] = trailArg[List[String]]()
  verify()
}

case class GrepCommand() extends Command {

}

/**
  * Takes a string and returns a command which corresponds to this string.
  */
object Command {
  def apply(name: String): Command = name match {
    case "echo" => EchoCommand()
    case "pwd" => PwdCommand()
    case "wc" => WcCommand()
    case "exit" => ExitCommand()
    case "cat" => CatCommand()
    case _@t => ProcessCommand(t)
  }
}

