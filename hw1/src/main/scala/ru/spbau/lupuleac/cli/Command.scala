package ru.spbau.lupuleac.cli

import sys.process._

trait Command {
  def execute(arguments: Argument*) : Output
}

case class EchoCommand() extends Command {
  override def execute(arguments: Argument*): Output = Output(arguments.map(x => x.asString).mkString(" "))
}

case class WcCommand() extends Command {
  override def execute(arguments: Argument*): Output = {
     val list = arguments.map(x => (x.asFile.split("\\s"), x.asFile.split("\n").length,
      x.asFile.getBytes().length, x.asString))
     Output(list.mkString("\n"))
  }
}

case class CatCommand() extends Command {
  override def execute(arguments: Argument*): Output = {
    Output(arguments.map(x => x.asFile).mkString("\n"))
  }
}

case class PwdCommand() extends Command {
  override def execute(arguments: Argument*): Output = Output(System.getProperty("user.dir"))
}

case class ExitCommand() extends Command {
  override def execute(arguments: Argument*): Output = {
    System.exit(0)
    Output("")
  }
}

case class ProcessCommand() extends Command {
  override def execute(arguments: Argument*): Output = {
    val sysCommand = arguments.map(x => x.asString).mkString(" ")
    Output(sysCommand !!)
  }
}

object Command {
  def apply(name : String) : Command = name match {
      case "echo" => EchoCommand()
      case "pwd" => PwdCommand()
      case "wc" => WcCommand()
      case "exit" => ExitCommand()
      case "cat" => CatCommand()
      case _ => ProcessCommand()
    }
}

