package ru.spbau.lupuleac.cli

import org.rogach.scallop._

import scala.collection.mutable
import scala.io.Source
import scala.sys.process._
import scala.util.matching.Regex

/**
  * A output of the command.
  * It is passed to the next command if the command was followed by pipe.
  *
  * @param text is the text which is returned by the command
  */
case class Output(text: String)

/**
  * Command in interpreter.
  * Output is an output of the previous command
  */
trait Command {
  val output: Output

  /*
   * Takes the arguments, executes the command and returns the output with the results.
   */
  def apply(arguments: String*): Output
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
  * Returns all arguments, separated by space
  */
case class EchoCommand(output: Output) extends Command {
  override def apply(arguments: String*): Output = Output(arguments.mkString(" "))
}

/**
  * Returns number of lines, words and bytes in each file provided
  * as an argument and total number of lines, words and bytes.
  */
case class WcCommand(output: Output) extends Command {
  def numberOfLines(str: String): Int = str.split("\n").length

  def numberOfBytes(str: String): Int = str.getBytes().length

  def numberOfWords(str: String): Int = str.split("\\s").length

  override def apply(arguments: String*): Output = {
    if (arguments.isEmpty) {
      return Output(List(numberOfLines(output.text), numberOfWords(output.text), numberOfBytes(output.text)).mkString(" "))
    }
    val list = arguments.map(x => (File(x), x)).map(x => (numberOfLines(x._1), numberOfWords(x._1), numberOfBytes(x._1), x._2))
    val totalLines = list.foldLeft(0)((x: Int, y: (Int, Int, Int, String)) => x + y._1)
    val totalWords = list.foldLeft(0)((x: Int, y: (Int, Int, Int, String)) => x + y._2)
    val totalBytes = list.foldLeft(0)((x: Int, y: (Int, Int, Int, String)) => x + y._3)
    val res = list.map(x => List(x._1.toString, x._2.toString, x._3.toString, x._4).mkString(" ").trim)
    val total = List(totalLines, totalWords, totalBytes).map(x => x.toString).mkString(" ") + " total"
    Output((res :+ total).mkString("\n"))
  }
}

/**
  * Returns a file (or files) contents.
  */
case class CatCommand(output: Output) extends Command {
  override def apply(arguments: String*): Output = {
    if (arguments.isEmpty) {
      return output
    }
    Output(arguments.map(x => File(x)).mkString("\n"))
  }
}

/**
  * Returns the current working directory (and ignores) it's arguments.
  */
case class PwdCommand(output: Output) extends Command {
  override def apply(arguments: String*): Output = Output(System.getProperty("user.dir"))
}

/**
  * Exits from the interpreter.
  */
case class ExitCommand(output: Output) extends Command {
  override def apply(arguments: String*): Output = {
    System.exit(0)
    Output("")
  }
}

/**
  * Makes a system call and executes the command with its arguments.
  *
  * @param command is an unknown command to be executed
  */
case class ProcessCommand(command: String, output: Output) extends Command {
  override def apply(arguments: String*): Output = {
    val args = arguments.mkString(" ")
    val sysCommand = command + " " + args
    Output(Process(sysCommand) !!)
  }
}


case class GrepConf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val ignoreCase: ScallopOption[Boolean] = toggle(name = "ignore-case", default = Some(false), descrYes = "Ignore case distinctions in both the PATTERN and the input files.")
  val wordRegex: ScallopOption[Boolean] = toggle(name = "word-regexp", default = Some(false), descrYes = "Select only those lines containing matches that form whole words.\n" +
    "The test is that the matching substring must either be at the beginning of the line, or preceded by a non-word constituent character. Similarly, it must be either at the end of the line or followed by a non-word constituent character. Word-constituent characters are letters, digits, and the underscore.")
  val afterContext: ScallopOption[Int] = opt[Int](short = 'A', descr = "Print NUM lines of trailing context after matching lines.", validate = x => x >= 0, argName = "NUM", default = Some(0))
  val pattern: ScallopOption[String] = trailArg[String](validate = x => try {
    x.r
    true
  } catch {
    case _: Exception => false
  })
  val files: ScallopOption[List[String]] = trailArg[List[String]](required = false)
  verify()
}

case class GrepCommand(output: Output) extends Command {

  def matchWord(pattern: Regex, line: String): Boolean = {
    val mi = pattern.findAllIn(line)
    while (mi.hasNext) {
      val m = mi.next
      if (m matches "[a-z|A-Z|0-9|_]*") {
        return true
      }
    }
    false
  }

  def lineToPrint(filename: String, line: String): String = if (filename == "") line else filename + ": " + line

  override def apply(arguments: String*): Output = {
    val conf = GrepConf(arguments)

    val files = if (conf.files.isEmpty) {
      List(("", output.text))
    } else {
      conf.files().map(x => (x, File(x)))
    }
    val pattern = conf.pattern().r
    val res = mutable.MutableList[String]()
    for ((filename, contents) <- files) {
      var num = 0
      for (line <- contents.split("\n")) {
        val lineToMatch = if (conf.ignoreCase()) line.toLowerCase else line
        val hasMatch = if (conf.wordRegex()) {
          matchWord(pattern, lineToMatch)
        } else {
          pattern.findFirstIn(lineToMatch).isDefined
        }
        if (hasMatch) {
          res += lineToPrint(filename, line)
          num = conf.afterContext()
        } else {
          if (num > 0) {
            res += lineToPrint(filename, line)
            num -= 1
          }
        }
      }
    }
    Output(res.mkString("\n"))
  }
}

/**
  * Takes a string and the output of the previous command and returns a command which corresponds to this string.
  */
object Command {
  def apply(name: String, output: Output): Command = name match {
    case "echo" => EchoCommand(output)
    case "pwd" => PwdCommand(output)
    case "wc" => WcCommand(output)
    case "exit" => ExitCommand(output)
    case "cat" => CatCommand(output)
    case _@t => ProcessCommand(t, output)
  }
}

