package ru.spbau.lupuleac.cli

import org.scalatest.{FlatSpec, Matchers}

class CommandTest extends FlatSpec with Matchers {
  "Echo command" should "print it's arguments" in {
    EchoCommand()(CommandLineArgument("a"), CommandLineArgument("b")).asFile should be("a b")
  }

  "Wc command" should "print number of lines, words and bytes in file" in {
    WcCommand()(CommandLineArgument("src/test/resources/a.txt")).asFile should be("1 2 11 src/test/resources/a.txt\n1 2 11 total")
  }

  "Wc command" should "also print total number for several files correctly" in {
    WcCommand()(Output("i'm new\nin this town"), CommandLineArgument("src/test/resources/a.txt")).asFile should be("2 5 20 \n1 2 11 src/test/resources/a.txt\n3 7 31 total")
  }

  "Cat command" should "print file content" in {
   CatCommand()(CommandLineArgument("src/test/resources/a.txt")).asFile should be("hello world")
  }

  "Grep config" should "evaluate arguments in this case correctly" in {
    val conf = GrepConf(List("-i", "-A 5", "[a]*", "file1", "file2"))
    conf.afterContext should be (Some(5))
  }
}
