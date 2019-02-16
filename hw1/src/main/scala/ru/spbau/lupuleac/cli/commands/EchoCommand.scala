package ru.spbau.lupuleac.cli.commands

/**
  * Returns all arguments, separated by space
  */
case class EchoCommand(stdin: Input, arguments: List[String]) extends Command {
  override def execute(): String = arguments.mkString(" ")

  override val name: String = "echo"

  override def isValid : Boolean = true
}
