package ru.spbau.lupuleac.cli

import org.scalatest.{FlatSpec, Matchers}

class CommandTest extends FlatSpec with Matchers {
  "Echo command" should "print it's arguments" in {
    EchoCommand()(CommandLineArgument("a"), CommandLineArgument("b")).asFile should be ("a b")
  }

  "Wc command" should "print number of words, lines and bytes in file" in {
    WcCommand()(CommandLineArgument("src/test/resources/a.txt")).asFile should be ("2 1 11 src/test/resources/a.txt\n2 1 11 total")
  }
}
