package ru.spbau.lupuleac.cli.commands

import ru.spbau.lupuleac.cli.Interpreter

/**
  * Returns the current working directory (and ignores it's arguments).
  */
case class PwdCommand(stdin: Input) extends Command {
  override def execute(interpreter: Interpreter): String = interpreter.curDir.toString

  override val name: String = "pwd"
  override val arguments: List[String] = List()

  override def isValid: Boolean = true
}
