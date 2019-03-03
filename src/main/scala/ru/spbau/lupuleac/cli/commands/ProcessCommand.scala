package ru.spbau.lupuleac.cli.commands

import scala.language.postfixOps
import scala.sys.process.Process
import scala.util.Try


/**
  * Makes a system call and executes the command with its arguments.
  *
  * @param command is an unknown command to be executed
  */
case class ProcessCommand(command: String, stdin: Input, arguments: List[String]) extends Command {
  override def execute(): Try[String] = {
    val prefix = if (System.getProperty("os.name").startsWith("Win")) "cmd /c " else ""
    val args = arguments.mkString(" ")
    val sysCommand = prefix + command + " " + args
    Try(Process(sysCommand) !!)
  }

  override val name: String = "process"

  override def isValid: Boolean = true
}