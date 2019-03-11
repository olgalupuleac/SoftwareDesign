package ru.spbau.lupuleac.cli.commands

import java.io.File

import scala.util.Try

/**
  * Returns number of lines, words and bytes in each file provided
  * as an argument and total number of lines, words and bytes.
  */
case class WcCommand(arguments: Seq[String]) extends Command {
  override val name: String = "wc"

  override def apply(stdin: Input): Try[String] = {
    if (arguments.isEmpty) {
      return Try {
        val str = stdin.get
        List(str.split(System.lineSeparator()).length, str.split("[\\s]+").count(s => s.nonEmpty),
          str.getBytes().length).mkString(" ")
      }
    }
    FileUtils(arguments).flatMap(list => {
      Try {
        val res = for ((filename, lines) <- list)
          yield (lines.length, lines.map(x => x.split("[\\s]+").count(s => s.nonEmpty)).sum, new File(filename).length().toInt, filename)
        if (arguments.length == 1) {
          format(res)
        } else {
          val total = res.foldLeft((0, 0, 0, "total"))((x, y) => (x._1 + y._1, x._2 + y._2, x._3 + y._3, "total"))
          format(res :+ total)
        }
      }
    })
  }

  /**
    * Transforms a list of tuples with info to string.
    *
    * @param list is a list to be transformed. Elements of list should be tuples (number of lines,
    *             number of words, number of bytes, filename).
    * @return a string representation of the list
    */
  def format(list: List[(Int, Int, Int, String)]): String =
    list.map { case (numberOfLines, numberOfWords, numberOfBytes, filename) =>
      s"$numberOfLines $numberOfWords $numberOfBytes $filename"
    }.mkString(System.lineSeparator())
}