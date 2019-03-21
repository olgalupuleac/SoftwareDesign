package ru.spbau.lupuleac.cli.commands

import com.joefkelley.argyle._
import ru.spbau.lupuleac.cli.commands.GrepCommand.GrepConfig

import scala.util.Try

object GrepCommand {

  /**
    * Parameters for grep command.
    *
    * @param ignoreCase   ignore case distinctions in  both  the  PATTERN  and
    *                     the  input
    *                     files
    * @param wordRegex    select only those  lines  containing  matches  that
    *                     form  whole
    *               words.   The  test is that the matching substring must either be
    *                     at the  beginning  of  the  line,  or  preceded  by  a  non-word
    *                     constituent  character.  Similarly, it must be either at the end
    *                     of the line or followed by  a  non-word  constituent  character.
    *                     Word-constituent   characters   are  letters,  digits,  and  the
    *                     underscore
    * @param afterContext print specified number  lines  of  trailing  context
    *                     after
    *                     matching  lines
    *                     Places   a  line  containing  a  group  separator  (--)  between
    *                     contiguous groups of matches.  With the  -o  or  --only-matching
    *                     option, this has no effect and a warning is given
    * @param files        grep  searches the named input files (or standard
    *                     input if no files are
    *                     named)
    * @param pattern      grep searches for files containing the given pattern
    */
  case class GrepConfig(
      ignoreCase: Boolean,
      wordRegex: Boolean,
      afterContext: Int,
      files: List[String],
      pattern: String
  )

  val conf: Arg[GrepConfig] = (flag("-i", "--ignore-case") and
    flag("-w", "--word-regexp") and
    optional[Int]("-A").default(0) and
    repeatedFree[String] and requiredFree[String]).as[GrepConfig]
}

/**
  * Returns lines matching the pattern.
  **/
case class GrepCommand(arguments: Seq[String]) extends Command {
  override val name: String = "grep"
  private val confOr: Try[GrepConfig] = GrepCommand.conf.parse(arguments: _*)

  override def apply(stdin: Input): Try[String] =
    Try {
      val conf = confOr.get
      if (conf.afterContext < 0) {
        throw new IllegalArgumentException(
          "Number of lines should not be a negative" +
            " number")
      }
      var pattern = conf.pattern
      if (conf.ignoreCase) {
        pattern = "(?i)" + pattern
      }
      if (conf.wordRegex) {
        pattern = "\\b" + pattern + "\\b"
      }
      if (conf.files.isEmpty) {
        matchLines(stdin.get.split(System.lineSeparator()), pattern, 0)
          .mkString(System.lineSeparator())
      } else {
        val files = FileUtils(conf.files).get
        files
          .flatMap {
            case (filename, lines) =>
              matchLines(lines, pattern, 0).map(line =>
                lineToPrint(filename, line))
          }
          .mkString(System.lineSeparator())
      }
    }

  /**
    * Takes a sequence of lines and returns lines
    * which satisfies command arguments
    *
    * @param lines        is a lines to be matched
    * @param pattern      is a pattern which we should find
    * @param afterContext is a number of lines after the previous match which
    *                     should be included
    * @return a lines for output
    */
  def matchLines(lines: Seq[String],
                 pattern: String,
                 afterContext: Int): List[String] = {
    if (lines.isEmpty) return List()
    val firstMatch = pattern.r findFirstMatchIn lines.head
    if (firstMatch.isDefined) {
      lines.head :: matchLines(lines.tail, pattern, confOr.get.afterContext)
    } else {
      if (afterContext > 0) {
        lines.head :: matchLines(lines.tail, pattern, afterContext - 1)
      } else {
        matchLines(lines.tail, pattern, 0)
      }
    }
  }

  /**
    *
    * @param filename is a file name
    * @param line     is a line to be printed
    * @return a correct line to be printed in the output
    */
  def lineToPrint(filename: String, line: String): String =
    if (printFileName) filename + ": " + line else line

  /**
    *
    * @return true if the file name should be printed before every line
    */
  def printFileName: Boolean =
    confOr.isSuccess && confOr.get.files.size > 1
}
