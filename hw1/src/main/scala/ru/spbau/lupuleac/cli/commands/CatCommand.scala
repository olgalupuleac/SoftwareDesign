package ru.spbau.lupuleac.cli.commands

import java.nio.file.Paths

import ru.spbau.lupuleac.cli.Interpreter

/**
  * Returns a file (or files) contents.
  */
case class CatCommand(stdin: Input, arguments: List[String]) extends Command {
  override val name: String = "cat"

  override def isValid: Boolean = !(arguments.isEmpty && stdin.isEmpty)

  override def execute(interpreter: Interpreter): String = {
    if (arguments.isEmpty) {
      return stdin.text
    }
    arguments.map(x => {
      FileUtils(interpreter.currentDirectory.resolve(x).toString)
    }).mkString("\n")
  }
}
