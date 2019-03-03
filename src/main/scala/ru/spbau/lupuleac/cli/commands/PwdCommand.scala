package ru.spbau.lupuleac.cli.commands

import scala.util.Try

/**
  * Returns the current working directory (and ignores it's arguments).
  */
case class PwdCommand(stdin: Input) extends Command {
  override def execute(): Try[String] = Try(System.getProperty("user.dir"))

  override val name: String = "pwd"
  override val arguments: List[String] = List()

  override def isValid: Boolean = true
}
