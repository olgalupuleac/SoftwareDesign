package ru.spbau.lupuleac.cli

import org.scalatest.{FlatSpec, Matchers}

class InterpreterTest extends FlatSpec with Matchers {
  "Interpreter" should "print arguments in echo command" in {
    val line = "echo x y z"
    val interpreter = new Interpreter()
    interpreter(line).asFile should be("x y z")
  }


  "Interpreter" should "substitute variables in echo command" in {
    val interpreter = new Interpreter()
    interpreter("x=1")
    interpreter("echo $x y z").asFile should be("1 y z")
  }

  "Interpreter" should "pass result as stdin to pipe" in {
    val interpreter = new Interpreter()
    println(interpreter("echo au | wc").asFile)
  }
}
