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
case class EchoCommand() extends Command {
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

