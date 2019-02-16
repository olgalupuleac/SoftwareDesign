package ru.spbau.lupuleac.cli.commands

import org.scalatest.{FlatSpec, Matchers}

class CommandTest extends FlatSpec with Matchers {
  "Echo command" should "print it's arguments" in {
    EchoCommand(EmptyInput(), List("a", "b"))() should be ("a b")
  }

  "Wc command" should "print number of lines, words and bytes in file" in {
    WcCommand(EmptyInput(), List("src/test/resources/a.txt"))() should be("1 2 11 src/test/resources/a.txt\n1 2 11 total")
  }

  "Wc command" should "also print total number for several files correctly" in {
  }

  "Cat command" should "print file content" in {
   CatCommand(EmptyInput(), List("src/test/resources/a.txt"))() should be ("hello world")
  }
}
