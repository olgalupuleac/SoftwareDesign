package ru.spbau.lupuleac.cli.commands

import scala.util.Try

/**
  * Returns all arguments, separated by space
  */
case class EchoCommand(arguments: Seq[String]) extends Command {
  override def apply(stdin: Input): Try[String] = Try(arguments.mkString(" "))

  override val name: String = "echo"
}
