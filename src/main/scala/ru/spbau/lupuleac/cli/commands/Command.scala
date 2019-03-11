package ru.spbau.lupuleac.cli.commands

import scala.io.Source
import scala.util.Try

/**
  * Trait which describes standard input, which is passed to the command.
  */
sealed trait Input {
  val isEmpty: Boolean
  def get: String
}

/**
  * Class which describes stdin with some text passed to the command which is preceded by pipe.
  */
case class InputWithText(text: String) extends Input {
  override val isEmpty: Boolean = false
  override def get: String = text
}

/**
  * Class which describes the input passed to the first command.
  */
case class EmptyInput() extends Input {
  override val isEmpty: Boolean = true
  override def get = throw new IllegalArgumentException("No input provided")
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
    * Arguments for a command.
    */
  val arguments: Seq[String]

  /**
    * Executes the command with it's arguments.
    *
    * @return the string which should be printed.
    */
  def apply(input: Input): Try[String]
}

/**
  * Takes the sequence of file names and returns its contents.
  */
object FileUtils {

  /**
    *
    * @param filenames is a sequence of file names
    * @return Success and list of filename and it's lines if all files were successfully read,
    *         Failure with exception otherwise.
    */
  def apply(filenames: Seq[String]): Try[List[(String, List[String])]] = Try {
    val res = for (filename <- filenames)
      yield
        (filename, using(io.Source.fromFile(filename)) { source =>
          (for (line <- source.getLines) yield line).toList
        })
    res.toList
  }

  /**
    * "Loan pattern" to close the resources automatically.
    *
    * @param r a resource to be closed
    * @param f a function to be applied to the resource
    * @tparam A a return type of the function
    * @return a result of applying function to the resources
    */
  def using[A](r: Source)(f: Source => A): A =
    try {
      f(r)
    } finally {
      r.close()
    }
}

/**
  * Takes a string and the output of the previous command and returns a command which corresponds to this string.
  */
object CommandFactory {
  def apply(name: String, args: Seq[String]): Command = name match {
    case "echo" => EchoCommand(args)
    case "pwd"  => PwdCommand()
    case "wc"   => WcCommand(args)
    case "exit" => ExitCommand()
    case "cat"  => CatCommand(args)
    case _ @t   => ProcessCommand(t, args)
  }
}
