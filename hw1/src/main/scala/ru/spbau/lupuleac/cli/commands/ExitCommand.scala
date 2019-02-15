package ru.spbau.lupuleac.cli.commands

/**
  * Exits from the interpreter.
  */
case class ExitCommand(stdin: Option[String]) extends Command {
  override def execute(): String = {
    System.exit(0)
    ""
  }

  override val name: String = "exit"

  override def validate(): Boolean = true

  override val arguments: List[String] = List()
}