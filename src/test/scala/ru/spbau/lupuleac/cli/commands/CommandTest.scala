package ru.spbau.lupuleac.cli.commands

import java.io.FileNotFoundException

import org.scalatest.TryValues._
import org.scalatest.{FlatSpec, Matchers}

class CommandTest extends FlatSpec with Matchers {
  def makeText(lines: String*): String = lines.mkString(System.lineSeparator())

  "Echo command" should "print it's arguments" in {
    val outOr = EchoCommand(EmptyInput(), List("a", "b"))()
    outOr.success.value should be("a b")
  }

  "Echo command" should "print empty string if there is no arguments" in {
    val outOr = EchoCommand(InputWithText("hello"), List())()
    outOr.success.value should be("")
  }

  "Wc command" should "print number of lines, words and bytes in file" in {
    val outOr = WcCommand(EmptyInput(), List("src/test/resources/a.txt"))()
    outOr.success.value should be("1 2 11 src/test/resources/a.txt")
  }

  "Wc command" should "also print total number for several files correctly" in {
    val expected = if (System.getProperty("os.name").startsWith("Win")) makeText("1 2 11 src/test/resources/a.txt",
      "5 5 25 src/test/resources/several_lines", "6 7 36 total") else makeText("1 2 11 src/test/resources/a.txt",
      "5 5 21 src/test/resources/several_lines", "6 7 32 total")
    val outOr = WcCommand(EmptyInput(), List("src/test/resources/a.txt", "src/test/resources/several_lines"))()
    outOr.success.value should be(expected)
  }

  "Wc command" should "print number of lines, words and bytes in file from stdin if no arguments are provided" in {
    val outOr = WcCommand(InputWithText("aaa a"), List())()
    outOr.success.value should be("1 2 5")
  }

  "Wc command" should "ignore stdin if the arguments are provided" in {
    val outOr = WcCommand(InputWithText("aaa a"), List("src/test/resources/a.txt"))()
    outOr.success.value should be("1 2 11 src/test/resources/a.txt")
  }

  "Wc command" should "print an error if arguments are incorrect" in {
    val outOr = WcCommand(EmptyInput(), List())()
    outOr.failure.exception should have message "Invalid arguments"
  }

  "Cat command" should "print file content" in {
    val outOr = CatCommand(EmptyInput(), List("src/test/resources/a.txt"))()
    outOr.success.value should be("hello world")
  }

  "Cat command" should "print the stdin if no arguments are provided" in {
    val outOr = CatCommand(InputWithText("hello from stdin"), List())()
    outOr.success.value should be("hello from stdin")
  }

  "Cat command" should "print contents of all files" in {
    val outOr = CatCommand(EmptyInput(), List("src/test/resources/a.txt", "src/test/resources/several_lines"))()
    outOr.success.value should be(makeText("hello world", "my", "lines", "in", "this", "file"))
  }

  "Cat command" should "ignore stdin if arguments are provided" in {
    val outOr = CatCommand(InputWithText("aa"), List("src/test/resources/several_lines"))()
    outOr.success.value should be(makeText("my", "lines", "in", "this", "file"))
  }

  "Process command" should "find a string in file" in {
    val command = if (System.getProperty("os.name").startsWith("Win")) "find \"i\" src/test/resources/several_lines" else "grep i src/test/resources/several_lines"
    val res = ProcessCommand(command, EmptyInput(), List())()
    res.success.value should include(List("lines", "in", "this", "file").mkString(System.lineSeparator()))
    res.success.value should not include "my"
  }

  "Pwd command" should "print a current directory" in {
    val outOr = PwdCommand(EmptyInput())()
    outOr.success.value should endWith("SoftwareDesign")
  }

  "Cat command" should "print what exception occured if file doesn't exist" in {
    val outOr = CatCommand(EmptyInput(), List("src/test/resources/a.txt", "src/test/resources/not_a_file"))()
    outOr.failure.exception shouldBe a[FileNotFoundException]
  }

  "Wc command" should "print what exception occured if file doesn't exist" in {
    val outOr = WcCommand(EmptyInput(), List("src/test/resources/a.txt", "src/test/resources/not_a_file"))()
    outOr.failure.exception shouldBe a[FileNotFoundException]
  }
}
