package ru.spbau.lupuleac.cli

import org.scalatest.{FlatSpec, Matchers}

class InterpreterTest extends FlatSpec with Matchers {
  "Interpreter" should "print arguments in echo command" in {
    val line = "echo x y z"
    val interpreter = new Interpreter()
    println(interpreter(line).asFile)
  }
}
