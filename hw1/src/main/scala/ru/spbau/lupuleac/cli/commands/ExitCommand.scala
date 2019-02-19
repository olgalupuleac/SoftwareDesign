package ru.spbau.lupuleac.cli.commands

import ru.spbau.lupuleac.cli.Interpreter

/**
  * Exits from the interpreter.
  */
case class ExitCommand(stdin: Input) extends Command {
  override def execute(interpreter: Interpreter): String = {
    System.exit(0)
    ""
  }

  override val name: String = "exit"

  override def isValid: Boolean = true

  override val arguments: List[String] = List()
}