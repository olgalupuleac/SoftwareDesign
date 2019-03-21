package ru.spbau.lupuleac.cli

import scala.io.StdIn

object Main {

  /**
    * The console interpreter itself
    *
    * @param args will be ignored
    */
  def main(args: Array[String]): Unit = {
    val interpreter = new Interpreter()
    while (true) {
      val line = StdIn.readLine()
      println(interpreter(line))
    }
  }
}
