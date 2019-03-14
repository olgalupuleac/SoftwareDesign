package ru.spbau.lupuleac.cli.commands

import scala.util.{Success, Try}

/**
  * Exits from the interpreter.
  */
case class ExitCommand() extends Command {
  override def apply(stdin: Input): Try[String] = {
    System.exit(0)
    Success("")
  }

  override val name: String = "exit"

  override val arguments: List[String] = List()
}
