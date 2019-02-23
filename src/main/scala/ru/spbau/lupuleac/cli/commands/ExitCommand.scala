package ru.spbau.lupuleac.cli.commands

import scala.util.{Success, Try}

/**
  * Exits from the interpreter.
  */
case class ExitCommand(stdin: Input) extends Command {
  override def execute(): Try[String] = {
    System.exit(0)
    Success("")
  }

  override val name: String = "exit"

  override def isValid: Boolean = true

  override val arguments: List[String] = List()
}