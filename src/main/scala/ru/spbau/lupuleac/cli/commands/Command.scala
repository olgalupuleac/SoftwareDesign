package ru.spbau.lupuleac.cli.commands

import scala.io.Source
import scala.util.{Failure, Try}

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
case class InputWithText(text: String) extends Input {
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
  def execute(): Try[String]


  /*
   * Executes the command if the arguments are correct.
   */
  def apply(): Try[String] = {
    if (isValid) {
      execute()
    } else {
      Failure(new IllegalArgumentException("Invalid arguments"))
    }
  }
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
      yield (filename, using(io.Source.fromFile(filename)) { source =>
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
  def apply(name: String, stdin: Input, args: List[String]): Command = name match {
    case "echo" => EchoCommand(stdin, args)
    case "pwd" => PwdCommand(stdin)
    case "wc" => WcCommand(stdin, args)
    case "exit" => ExitCommand(stdin)
    case "cat" => CatCommand(stdin, args)
    case _@t => ProcessCommand(t, stdin, args)
  }
}