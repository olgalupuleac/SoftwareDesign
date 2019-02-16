package ru.spbau.lupuleac.cli.commands

/**
  * Returns number of lines, words and bytes in each file provided
  * as an argument and total number of lines, words and bytes.
  */
case class WcCommand(stdin: Input, arguments: List[String]) extends Command {
  def numberOfLines(str: String): Int = str.split("\n").length

  def numberOfBytes(str: String): Int = str.getBytes().length

  def numberOfWords(str: String): Int = str.split("\\s").length

  override def execute(): String = {
    if (arguments.isEmpty) {
      return List(numberOfLines(stdin.text), numberOfWords(stdin.text), numberOfBytes(stdin.text)).mkString(" ")
    }
    val list = arguments.map(x => (FileUtils(x), x)).map(x => (numberOfLines(x._1), numberOfWords(x._1), numberOfBytes(x._1), x._2))
    val totalLines = list.foldLeft(0)((x: Int, y: (Int, Int, Int, String)) => x + y._1)
    val totalWords = list.foldLeft(0)((x: Int, y: (Int, Int, Int, String)) => x + y._2)
    val totalBytes = list.foldLeft(0)((x: Int, y: (Int, Int, Int, String)) => x + y._3)
    val res = list.map(x => List(x._1.toString, x._2.toString, x._3.toString, x._4).mkString(" ").trim)
    val total = List(totalLines, totalWords, totalBytes).map(x => x.toString).mkString(" ") + " total"
    (res :+ total).mkString("\n")
  }

  override val name: String = "wc"

  override def isValid: Boolean = !(arguments.isEmpty && stdin.isEmpty)
}