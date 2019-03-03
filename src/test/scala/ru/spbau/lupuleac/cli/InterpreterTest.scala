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
    interpreter("echo au | wc") should be("1 1 2")
  }

  "Interpreter" should "read file contents and pass it to next command" in {
    val interpreter = new Interpreter()
    interpreter("cat src/test/resources/a.txt | wc") should be("1 2 11")
  }

  "Interpreter" should "pass stdin to echo and return empty string" in {
    val interpreter = new Interpreter()
    interpreter("cat src/test/resources/a.txt | echo") should be("")
  }

  "Interpreter" should "be able to deal with this trash" in {
    val interpreter = new Interpreter()
    interpreter("x=ho")
    interpreter("e'c'\"$x\" a") should be ("a")
  }

  "Interpreter" should "treat words in double quotes as one argument" in {
    val interpreter = new Interpreter()
    interpreter("echo \"my    world\"     is") should be("my    world is")
  }

  "Interpreter" should "execute grep correctly" in {
    val interpreter = new Interpreter()
    interpreter("echo AAA | grep -i a") should be("AAA")
  }
}
