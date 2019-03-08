package ru.spbau.lupuleac.cli.commands

import java.io.FileNotFoundException

import org.scalatest.TryValues._
import org.scalatest.{FlatSpec, Matchers}

class CommandTest extends FlatSpec with Matchers {
  def makeText(lines: String*): String = lines.mkString(System.lineSeparator())

  "Echo command" should "print it's arguments" in {
    val outOr = EchoCommand(List("a", "b"))(EmptyInput())
    outOr.success.value should be("a b")
  }

  "Echo command" should "print empty string if there is no arguments" in {
    val outOr = EchoCommand(List())(InputWithText("hello"))
    outOr.success.value should be("")
  }

  "Wc command" should "print number of lines, words and bytes in file" in {
    val outOr = WcCommand(List("src/test/resources/a.txt"))(EmptyInput())
    outOr.success.value should be("1 2 11 src/test/resources/a.txt")
  }

  "Wc command" should "also print total number for several files correctly" in {
    val expected = makeText("1 2 11 src/test/resources/a.txt",
      "5 5 21 src/test/resources/several_lines", "6 7 32 total")
    val outOr = WcCommand(List("src/test/resources/a.txt", "src/test/resources/several_lines"))(EmptyInput())
    outOr.success.value should be(expected)
  }

  "Wc command" should "calculate number of words if the line starts with space" in {
    val outOr = WcCommand(List("src/test/resources/spaces.txt"))(EmptyInput())
    outOr.success.value should be("5 10 59 src/test/resources/spaces.txt")
  }

  "Wc command" should "print number of lines, words and bytes in file from stdin if no arguments are provided" in {
    val outOr = WcCommand(List())(InputWithText("aaa a"))
    outOr.success.value should be("1 2 5")
  }

  "Wc command" should "ignore stdin if the arguments are provided" in {
    val outOr = WcCommand(List("src/test/resources/a.txt"))(InputWithText("aaa a"))
    outOr.success.value should be("1 2 11 src/test/resources/a.txt")
  }

  "Wc command" should "print an error if arguments are incorrect" in {
    val outOr = WcCommand(List())(EmptyInput())
    outOr.failure.exception should have message "No input provided"
  }

  "Cat command" should "print file content" in {
    val outOr = CatCommand(List("src/test/resources/a.txt"))(EmptyInput())
    outOr.success.value should be("hello world")
  }

  "Cat command" should "print the stdin if no arguments are provided" in {
    val outOr = CatCommand(List())(InputWithText("hello from stdin"))
    outOr.success.value should be("hello from stdin")
  }

  "Cat command" should "print contents of all files" in {
    val outOr = CatCommand(List("src/test/resources/a.txt", "src/test/resources/several_lines"))(EmptyInput())
    outOr.success.value should be(makeText("hello world", "my", "lines", "in", "this", "file"))
  }

  "Cat command" should "ignore stdin if arguments are provided" in {
    val outOr = CatCommand(List("src/test/resources/several_lines"))(InputWithText("aa"))
    outOr.success.value should be(makeText("my", "lines", "in", "this", "file"))
  }

  "Process command" should "find a string in file" in {
    val command = if (System.getProperty("os.name").startsWith("Win")) "find \"i\" src/test/resources/several_lines" else "grep i src/test/resources/several_lines"
    val res = ProcessCommand(command, List())(EmptyInput())
    res.success.value should include(List("lines", "in", "this", "file").mkString(System.lineSeparator()))
    res.success.value should not include "my"
  }

  "Pwd command" should "print a current directory" in {
    val outOr = PwdCommand()(EmptyInput())
    outOr.success.value should endWith("SoftwareDesign")
  }

  "Cat command" should "print what exception occured if file doesn't exist" in {
    val outOr = CatCommand(List("src/test/resources/a.txt", "src/test/resources/not_a_file"))(EmptyInput())
    outOr.failure.exception shouldBe a[FileNotFoundException]
  }

  "Wc command" should "print what exception occured if file doesn't exist" in {
    val outOr = WcCommand(List("src/test/resources/a.txt", "src/test/resources/not_a_file"))(EmptyInput())
    outOr.failure.exception shouldBe a[FileNotFoundException]
  }
}
