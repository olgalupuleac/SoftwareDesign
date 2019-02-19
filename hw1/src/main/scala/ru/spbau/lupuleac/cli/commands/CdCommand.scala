package ru.spbau.lupuleac.cli.commands

import java.nio.file
import java.nio.file.Paths

import ru.spbau.lupuleac.cli.Interpreter

/**
  * Changes current directory
  */
case class CdCommand(stdin: Input, arguments: List[String]) extends Command {
  override val name: String = "cd"

  override def isValid: Boolean = true

  override def execute(interpreter: Interpreter): String = {
    val targetPath : String = if (arguments.isEmpty) "/." else arguments.head
    interpreter.curDir = if (Paths.get(targetPath).isAbsolute) {
       Paths.get(targetPath)
    } else {
      Paths.get(interpreter.curDir.toString, targetPath)
    }
    ""
  }
}
