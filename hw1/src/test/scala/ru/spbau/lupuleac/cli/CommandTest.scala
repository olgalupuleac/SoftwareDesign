package ru.spbau.lupuleac.cli

import org.scalatest.{FlatSpec, Matchers}

class CommandTest extends FlatSpec with Matchers {
  "Echo command" should "print it's arguments" in {
    EchoCommand(Output(""))("a", "b").text should be("a b")
  }

  "Wc command" should "print number of lines, words and bytes in file" in {
    WcCommand(Output(""))("src/test/resources/a.txt").text should be("1 2 11 src/test/resources/a.txt\n1 2 11 total")
  }

  "Wc command" should "also print total number for several files correctly" in {
  }

  "Cat command" should "print file content" in {
   CatCommand(Output(""))("src/test/resources/a.txt").text should be ("hello world")
  }
}
