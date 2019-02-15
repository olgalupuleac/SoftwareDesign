package ru.spbau.lupuleac.cli.commands

import scala.io.Source


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
  val stdin: Option[String]

  /**
    * Arguments for a command.
    */
  val arguments: List[String]

  /**
    * Checks if the given arguments are correct.
    *
    * @return true if the arguments are correct
    */
  def validate(): Boolean

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
    if (validate()) {
      execute()
    } else {
      "bash: " + name + ": invalid arguments"
    }
  }
}


/**
  * Takes the file name and returns its contents.
  */
object File {
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
  def apply(name: String, stdin: Option[String], args: List[String]): Command = name match {
    case "echo" => EchoCommand(stdin, args)
    case "pwd" => PwdCommand(stdin)
    case "wc" => WcCommand(stdin, args)
    case "exit" => ExitCommand(stdin)
    case "cat" => CatCommand(stdin, args)
    case "grep" => GrepCommand(stdin, args)
    case _@t => ProcessCommand(t, stdin, args)
  }
}

