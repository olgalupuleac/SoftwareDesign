package ru.spbau.lupuleac.cli.commands

/**
  * Returns the current working directory (and ignores it's arguments).
  */
case class PwdCommand(stdin: Option[String]) extends Command {
  override def execute(): String = System.getProperty("user.dir")

  override val name: String = "pwd"
  override val arguments: List[String] = List()

  override def validate(): Boolean = true
}
