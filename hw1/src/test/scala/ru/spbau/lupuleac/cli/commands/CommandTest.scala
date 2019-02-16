package ru.spbau.lupuleac.cli.commands

import org.scalatest.{FlatSpec, Matchers}

class CommandTest extends FlatSpec with Matchers {
  "Echo command" should "print it's arguments" in {
    EchoCommand(EmptyInput(), List("a", "b"))() should be("a b")
  }

  "Echo command" should "print empty string if there is no arguments" in {
    EchoCommand(Stdin("hello"), List())() should be("")
  }

  "Wc command" should "print number of lines, words and bytes in file" in {
    WcCommand(EmptyInput(), List("src/test/resources/a.txt"))() should be("1 2 11 src/test/resources/a.txt\n1 2 11 total")
  }

  "Wc command" should "also print total number for several files correctly" in {
    WcCommand(EmptyInput(), List("src/test/resources/a.txt", "src/test/resources/several_lines"))() should be("1 2 11 src/test/resources/a.txt\n5 5 21 src/test/resources/several_lines\n6 7 32 total")
  }

  "Wc command" should "print number of lines, words and bytes in file from stdin if no arguments are provided" in {
    WcCommand(Stdin("aaa a"), List())() should be("1 2 5")
  }

  "Wc command" should "ignore stdin if the arguments are provided" in {
    WcCommand(Stdin("aaa a"), List("src/test/resources/a.txt"))() should be("1 2 11 src/test/resources/a.txt\n1 2 11 total")
  }

  "Wc command" should "print an error if arguments are incorrect" in {
    WcCommand(EmptyInput(), List())() should be("bash: wc: invalid arguments")
  }

  "Cat command" should "print file content" in {
    CatCommand(EmptyInput(), List("src/test/resources/a.txt"))() should be("hello world")
  }

  "Cat command" should "print the stdin if no arguments are provided" in {
    CatCommand(Stdin("hello from stdin"), List())() should be("hello from stdin")
  }

  "Cat command" should "print contents of all files" in {
    CatCommand(EmptyInput(), List("src/test/resources/a.txt", "src/test/resources/several_lines"))() should be("hello world\nmy\nlines\nin\nthis\nfile")
  }

  "Cat command" should "ignore stdin if arguments are provided" in {
    CatCommand(Stdin("aa"), List("src/test/resources/several_lines"))() should be("my\nlines\nin\nthis\nfile")
  }

  "Process command" should "find a string in file" in {
    val command = if(System.getProperty("os.name").startsWith("Win")) "find \"i\" src/test/resources/several_lines" else "grep i src/test/resources/several_lines"
    val res = ProcessCommand(command, EmptyInput(), List())()
    res should include (List("lines", "in", "this", "file").mkString(System.lineSeparator()))
    res should not include "my"
  }

  "Pwd command" should "print a current directory" in {
    PwdCommand(EmptyInput())() should endWith ("hw1")
  }
}
