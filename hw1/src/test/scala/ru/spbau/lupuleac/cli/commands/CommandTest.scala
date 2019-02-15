package ru.spbau.lupuleac.cli.commands

import org.scalatest.{FlatSpec, Matchers}

class CommandTest extends FlatSpec with Matchers {
  "Echo command" should "print it's arguments" in {
    EchoCommand(None, List("a", "b"))() should be ("a b")
  }

  "Wc command" should "print number of lines, words and bytes in file" in {
    WcCommand(None, List("src/test/resources/a.txt"))() should be("1 2 11 src/test/resources/a.txt\n1 2 11 total")
  }

  "Wc command" should "also print total number for several files correctly" in {
  }

  "Cat command" should "print file content" in {
   CatCommand(None, List("src/test/resources/a.txt"))() should be ("hello world")
  }

  "Grep config" should "evaluate arguments in this case correctly" in {
    val conf = GrepConf(List("-i", "-A", "5", "[a]*", "file1", "file2"))
    conf.afterContext() should equal (5)
    conf.ignoreCase() should equal (true)
    conf.wordRegex() should equal (false)
    conf.pattern() should equal ("[a]*")
    conf.files() should equal (List("file1", "file2"))
  }
}
