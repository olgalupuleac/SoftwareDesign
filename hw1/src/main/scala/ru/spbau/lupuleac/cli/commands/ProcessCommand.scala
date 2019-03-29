package ru.spbau.lupuleac.cli.commands

import java.io.File

import ru.spbau.lupuleac.cli.Interpreter

import scala.sys.process.Process


/**
  * Makes a system call and executes the command with its arguments.
  *
  * @param command is an unknown command to be executed
  */
case class ProcessCommand(command: String, stdin: Input, arguments: List[String]) extends Command {
  override def execute(interpreter: Interpreter): String = {
    val prefix = if (System.getProperty("os.name").startsWith("Win")) "cmd /c " else ""
    val args = arguments.mkString(" ")
    val sysCommand = prefix + command + " " + args
    Process(sysCommand, new File(interpreter.currentDirectory.toString)) !!
  }

  override val name: String = "process"

  override def isValid : Boolean = true
}