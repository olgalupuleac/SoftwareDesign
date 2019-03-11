package ru.spbau.lupuleac.cli.commands

import scala.util.{Success, Try}

/**
  * Returns a file (or files) contents.
  */
case class CatCommand(arguments: Seq[String]) extends Command {
  override val name: String = "cat"

  override def apply(stdin: Input): Try[String] = {
    if (arguments.isEmpty) {
      return Try(stdin.get)
    }
    val files = FileUtils(arguments)
    files.flatMap(t =>
      Try(t.flatMap(x => x._2).mkString(System.lineSeparator())))
  }
}
