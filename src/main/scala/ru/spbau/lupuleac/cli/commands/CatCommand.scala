package ru.spbau.lupuleac.cli.commands


import scala.util.{Success, Try}

/**
  * Returns a file (or files) contents.
  */
case class CatCommand(stdin: Input, arguments: List[String]) extends Command {
  override val name: String = "cat"

  override def isValid: Boolean = !(arguments.isEmpty && stdin.isEmpty)

  override def execute(): Try[String] = {
    if (arguments.isEmpty) {
      return Success(stdin.text)
    }
    val files = FileUtils(arguments)
    files.flatMap(t => Success(t.flatMap(x => x._2).mkString(System.lineSeparator())))
  }
}
