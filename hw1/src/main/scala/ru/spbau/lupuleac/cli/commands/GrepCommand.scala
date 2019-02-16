package ru.spbau.lupuleac.cli.commands

import org.rogach.scallop.{ScallopConf, ScallopOption}

import scala.collection.mutable
import scala.util.matching.Regex

/**
  * Checks if the provided arguments are correct.
  *
  * @param arguments is arguments to be checked.
  */
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

/**
  * Returns lines matching the pattern.
  **/
case class GrepCommand(stdin: Input, arguments: List[String]) extends Command {
  private var conf: GrepConf = _

  override def isValid: Boolean = {
    try {
      conf = GrepConf(arguments)
      if (conf.files.isEmpty && stdin.isEmpty) {
        return false
      }
      true
    } catch {
      case _: Exception => false
    }
  }

  /**
    * Checks if the line contains words which match the pattern.
    *
    * @param pattern is a pattern which occurrences should be checked
    * @param line    is a line to be checked
    * @return if the line contains a word which matches the pattern.
    */
  def matchWord(pattern: Regex, line: String): Boolean = {
    val words = line.split("[^(a-z|A-Z|0-9|_)]+")
    for (word <- words) {
      word match {
        case pattern() => return true
      }
    }
    false
  }

  /**
    *
    * @param filename is a file name
    * @param line     is a line to be printed
    * @return a correct line to be printed in the output
    */
  def lineToPrint(filename: String, line: String): String = if (filename == "") line else filename + ": " + line

  override def execute(): String = {
    val files = if (conf.files.isEmpty) {
      List(("", stdin.text))
    } else {
      conf.files().map(x => (x, FileUtils(x)))
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
    res.mkString("\n")
  }

  override val name: String = "grep"
}