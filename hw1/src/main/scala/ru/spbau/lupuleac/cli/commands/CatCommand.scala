package ru.spbau.lupuleac.cli.commands


import scala.util.{Failure, Success}

/**
  * Returns a file (or files) contents.
  */
case class CatCommand(stdin: Input, arguments: List[String]) extends Command {
  override val name: String = "cat"

  override def isValid: Boolean = !(arguments.isEmpty && stdin.isEmpty)

  override def execute(): String = {
    if (arguments.isEmpty) {
      return stdin.text
    }
    val files = FileUtils(arguments)
    files match {
      case Failure(s) => "bash: " + name + ": " + s
      case Success(t) => t.flatMap(x => x._2).mkString(System.lineSeparator())
    }
  }
}
