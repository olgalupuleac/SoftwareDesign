package ru.spbau.lupuleac.cli

import scala.sys.process._

trait Command {
  def apply(arguments: Argument*): Output
}

case class EchoCommand() extends Command {
  override def apply(arguments: Argument*): Output = Output(arguments.map(x => x.asString).mkString(" "))
}

case class WcCommand() extends Command {
  override def apply(arguments: Argument*): Output = {
    val list = arguments.map(x => (x.asFile.split("\\s").length, x.asFile.split("\n").length,
      x.asFile.getBytes().length, x.asString))
    val totalWords = list.foldLeft(0)((x: Int, y: (Int, Int, Int, String)) => x + y._1)
    val totalLines = list.foldLeft(0)((x: Int, y: (Int, Int, Int, String)) => x + y._2)
    val totalBytes = list.foldLeft(0)((x: Int, y: (Int, Int, Int, String)) => x + y._3)
    val res = list.map(x => List(x._1.toString, x._2.toString, x._3.toString, x._4).mkString(" "))
    val total = List(totalWords, totalLines, totalBytes).map(x => x.toString).mkString(" ") + " total"
    Output((res :+ total).mkString("\n"))
  }
}

case class CatCommand() extends Command {
  override def apply(arguments: Argument*): Output = {
    Output(arguments.map(x => x.asFile).mkString("\n"))
  }
}

case class PwdCommand() extends Command {
  override def apply(arguments: Argument*): Output = Output(System.getProperty("user.dir"))
}

case class ExitCommand() extends Command {
  override def apply(arguments: Argument*): Output = {
    System.exit(0)
    Output("")
  }
}

case class ProcessCommand(command: String) extends Command {
  override def apply(arguments: Argument*): Output = {
    val args = arguments.map(x => x.asString).mkString(" ")
    val sysCommand = command + " " + args
    Output(sysCommand !!)
  }
}

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

