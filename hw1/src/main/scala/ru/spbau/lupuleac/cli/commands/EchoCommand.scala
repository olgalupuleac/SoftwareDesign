package ru.spbau.lupuleac.cli.commands

import ru.spbau.lupuleac.cli.Interpreter

/**
  * Returns all arguments, separated by space
  */
case class EchoCommand(stdin: Input, arguments: List[String]) extends Command {
  override def execute(interpreter: Interpreter): String = arguments.mkString(" ")

  override val name: String = "echo"

  override def isValid: Boolean = true
}
