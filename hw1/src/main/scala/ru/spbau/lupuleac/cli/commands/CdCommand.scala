package ru.spbau.lupuleac.cli.commands

import java.io.File
import java.nio.file.Paths

import ru.spbau.lupuleac.cli.Interpreter

/**
  * Changes current directory
  */
case class CdCommand(stdin: Input, arguments: List[String]) extends Command {
  override val name: String = "cd"

  override def isValid: Boolean = true

  override def execute(interpreter: Interpreter): String = {
    if (arguments.size > 1) return throw new Exception("Too much arguments")
    val targetPath : String = if (arguments.isEmpty) System.getProperty("user.home") else arguments.head
    val newDirectory = if (Paths.get(targetPath).isAbsolute) {
       Paths.get(targetPath).normalize()
    } else {
      interpreter.currentDirectory.resolve(targetPath).normalize()
    }
    val file = new File(newDirectory.toString)
    if (file.exists() && file.isDirectory) {
      interpreter.currentDirectory = newDirectory
      return ""
    }
    "bash: No such directory " + newDirectory.toString
  }
}
