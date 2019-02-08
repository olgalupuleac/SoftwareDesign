package ru.spbau.lupuleac.cli

import scala.io.Source

/**
  * Can be passed to the command
  */
trait Argument {
  /*
  The argument is interpreted as file.
  Returns string representation of the argument.
   */
  def asFile: String

  /*
  The argument is interpreted as file.
  Returns file contents
   */
  def asString: String
}

/**
  * Argument from the command line.
  * @param text is an argument itself
  */
case class CommandLineArgument(text: String) extends Argument {
  override def asFile: String = {
    val file = Source.fromFile(text)
    val res = file.getLines.mkString("\n")
    file.close()
    res
  }

  override def asString: String = text
}

/**
  * A output of the command.
  * It is passed to the next command if the command was followed by pipe.
  * @param text is the text which is returned by the command
  */
case class Output(text: String) extends Argument {
  override def asFile: String = text

  // > echo x | echo -- empty output
  override def asString: String = ""
}