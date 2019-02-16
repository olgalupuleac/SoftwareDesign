package ru.spbau.lupuleac.cli.commands

import scala.io.Source

/**
  * Trait which describes standard input, which is passed to the command.
  */
sealed trait Input {
  val isEmpty: Boolean
  val text: String
}

/**
  * Class which describes stdin with some text passed to the command which is preceded by pipe.
  */
case class Stdin(text: String) extends Input {
  override val isEmpty: Boolean = false
}

/**
  * Class which describes the input passed to the first command.
  */
case class EmptyInput() extends Input {
  override val isEmpty: Boolean = true
  val text = ""
}

/**
  * Command in interpreter.
  * Output is an output of the previous command
  */
trait Command {
  /**
    * Name of the command
    */
  val name: String
  /**
    * Stdin (Some(String) if the command is preceded by pipe).
    */
  val stdin: Input

  /**
    * Arguments for a command.
    */
  val arguments: List[String]

  /**
    * Checks if the given arguments are correct.
    **/
  def isValid: Boolean

  /**
    * Executes the command with it's arguments.
    *
    * @return the string which should be printed.
    */
  def execute(): String


  /*
   * Executes the command if the arguments are correct.
   */
  def apply(): String = {
    if (isValid) {
      execute()
    } else {
      "bash: " + name + ": invalid arguments"
    }
  }
}


/**
  * Takes the file name and returns its contents.
  */
object FileUtils {
  def apply(str: String): String = {
    val file = Source.fromFile(str)
    val res = file.getLines.mkString("\n")
    file.close()
    res
  }
}


/**
  * Takes a string and the output of the previous command and returns a command which corresponds to this string.
  */
object CommandFactory {
  def apply(name: String, stdin: Input, args: List[String]): Command = name match {
    case "echo" => EchoCommand(stdin, args)
    case "pwd" => PwdCommand(stdin)
    case "wc" => WcCommand(stdin, args)
    case "exit" => ExitCommand(stdin)
    case "cat" => CatCommand(stdin, args)
    case "grep" => GrepCommand(stdin, args)
    case _@t => ProcessCommand(t, stdin, args)
  }
}