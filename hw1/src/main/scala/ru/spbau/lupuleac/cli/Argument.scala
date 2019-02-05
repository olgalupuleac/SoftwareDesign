package ru.spbau.lupuleac.cli

import scala.io.Source

trait Argument {
  def asFile : String
  def asString : String
}

case class CommandLineArgument(text : String) extends Argument  {
   override def asFile : String  = {
    val file = Source.fromFile(text)
    val res = file.getLines.mkString("\n")
    file.close()
    res
  }

  override def asString : String = text
}


object CommandLineArgument {
  def apply(text: String): CommandLineArgument = new CommandLineArgument(text)
}

class Output(text : String) extends Argument {
  override def asFile: String = text
  override def asString : String = ""
}

object Output {
  def apply(text: String): Output = new Output(text)
}

