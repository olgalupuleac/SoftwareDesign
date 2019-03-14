package ru.spbau.lupuleac.cli.commands

import org.rogach.scallop.{ScallopConf, ScallopOption}

import scala.util.{Failure, Success, Try}

/**
  * Checks if the provided arguments are correct.
  *
  * @param arguments is arguments to be checked.
  */
case class GrepConf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val ignoreCase: ScallopOption[Boolean] = toggle(
    name = "ignore-case",
    default = Some(false),
    descrYes =
      "Ignore case distinctions in both the PATTERN and the input files.")
  val wordRegex: ScallopOption[Boolean] = toggle(
    name = "word-regexp",
    default = Some(false),
    descrYes = "Select only those lines containing matches that form whole words.\n" +
      "The test is that the matching substring must either be at the beginning of the line, or preceded by a non-word constituent character. Similarly, it must be either at the end of the line or followed by a non-word constituent character. Word-constituent characters are letters, digits, and the underscore."
  )
  val afterContext: ScallopOption[Int] = opt[Int](
    short = 'A',
    descr = "Print NUM lines of trailing context after matching lines.",
    validate = x => x >= 0,
    argName = "NUM",
    default = Some(0))
  val pattern: ScallopOption[String] = trailArg[String](validate = x =>
    try {
      x.r
      true
    } catch {
      case _: Exception => false
  })
  val files: ScallopOption[List[String]] =
    trailArg[List[String]](required = false)
  verify()
}

/**
  * Returns lines matching the pattern.
  **/
case class GrepCommand(arguments: Seq[String]) extends Command {
  override val name: String = "grep"
  private val confOr: Try[GrepConf] = Try(GrepConf(arguments))

  override def apply(stdin: Input): Try[String] = {
    confOr match {
      case Failure(e) => Failure(e)
      case Success(conf) =>
        Try {
          var pattern = conf.pattern()
          if (conf.ignoreCase()) {
            pattern = "(?i)" + pattern
          }
          if (conf.wordRegex()) {
            pattern = "\b" + pattern + "\b"
          }
          if (conf.files.isEmpty) {
            matchLines(stdin.get.split(System.lineSeparator()), pattern, 0)
              .mkString(System.lineSeparator())
          } else {
            val files = FileUtils(conf.files()).get
            files
              .map {
                case (filename, lines) =>
                  matchLines(lines, pattern, 0).map(line =>
                    lineToPrint(filename, line))
              }
              .mkString(System.lineSeparator())
          }
        }

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
      lines.head :: matchLines(lines.tail,
                               pattern,
                               confOr.get
                                 .afterContext())
    } else {
      matchLines(lines.tail, pattern, Math.max(0, afterContext - 1))
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
    confOr.isSuccess && confOr.get.files.isDefined &&
      confOr.get.files().size > 1
}