package ru.spbau.lupuleac.cli

import scala.io.Source
import scala.sys.process._

/**
  * A output of the command.
  * It is passed to the next command if the command was followed by pipe.
  *
  * @param text is the text which is returned by the command
  */
case class Output(text: String)

/**
  * Command in interpreter.
  */
trait Command {
  val output: Output

  /*
   * Takes the arguments, executes the command and returns the output with the results.
   */
  def apply(arguments: String*): Output
}

object File {
  def apply(str: String): String = {
    val file = Source.fromFile(str)
    val res = file.getLines.mkString("\n")
    file.close()
    res
  }
}

/**
  * Returns all arguments, separated by space
  */
case class EchoCommand(output: Output) extends Command {
  override def apply(arguments: String*): Output = Output(arguments.mkString(" "))
}

/**
  * Returns number of lines, words and bytes in each file provided
  * as an argument and total number of lines, words and bytes.
  */
case class WcCommand(output: Output) extends Command {
  def numberOfLines(str: String): Int = str.split("\n").length

  def numberOfBytes(str: String): Int = str.getBytes().length

  def numberOfWords(str: String): Int = str.split("\\s").length

  override def apply(arguments: String*): Output = {
    if (arguments.isEmpty) {
      return Output(List(numberOfLines(output.text), numberOfWords(output.text), numberOfBytes(output.text)).mkString(" "))
    }
    val list = arguments.map(x => (File(x), x)).map(x => (numberOfLines(x._1), numberOfWords(x._1), numberOfBytes(x._1), x._2))
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
case class CatCommand(output: Output) extends Command {
  override def apply(arguments: String*): Output = {
    if (arguments.isEmpty) {
      return output
    }
    Output(arguments.map(x => File(x)).mkString("\n"))
  }
}

/**
  * Returns the current working directory (and ignores) it's arguments.
  */
case class PwdCommand(output: Output) extends Command {
  override def apply(arguments: String*): Output = Output(System.getProperty("user.dir"))
}

/**
  * Exits from the interpreter.
  */
case class ExitCommand(output: Output) extends Command {
  override def apply(arguments: String*): Output = {
    System.exit(0)
    Output("")
  }
}

/**
  * Makes a system call and executes the command with its arguments.
  *
  * @param command is an unknown command to be executed
  */
case class ProcessCommand(command: String, output: Output) extends Command {
  override def apply(arguments: String*): Output = {
    val args = arguments.mkString(" ")
    val sysCommand = command + " " + args
    Output(Process(sysCommand) !!)
  }
}

/**
  * Takes a string and returns a command which corresponds to this string.
  */
object Command {
  def apply(name: String, output: Output): Command = name match {
    case "echo" => EchoCommand(output)
    case "pwd" => PwdCommand(output)
    case "wc" => WcCommand(output)
    case "exit" => ExitCommand(output)
    case "cat" => CatCommand(output)
    case _@t => ProcessCommand(t, output)
  }
}

