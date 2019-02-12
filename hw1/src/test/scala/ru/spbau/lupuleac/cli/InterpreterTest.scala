package ru.spbau.lupuleac.cli

import org.scalatest.{FlatSpec, Matchers}

class InterpreterTest extends FlatSpec with Matchers {
  "Interpreter" should "print arguments in echo command" in {
    val line = "echo x y z"
    val interpreter = new Interpreter()
    interpreter(line) should be("x y z")
  }


  "Interpreter" should "substitute variables in echo command" in {
    val interpreter = new Interpreter()
    interpreter("x=1")
    interpreter("echo $x y z") should be("1 y z")
  }

  "Interpreter" should "pass result as stdin to pipe" in {
    val interpreter = new Interpreter()
    interpreter("echo au | wc") should be("1 1 2\n1 1 2 total")
  }

  "Interpreter" should "read file contents and pass it to next command" in {
    val interpreter = new Interpreter()
    interpreter("cat src/test/resources/a.txt | wc") should be("1 2 11\n1 2 11 total")
  }

  "Interpreter" should "pass stdin to echo and return empty string" in {
    val interpreter = new Interpreter()
    interpreter("cat src/test/resources/a.txt | echo") should be("")
  }


}
