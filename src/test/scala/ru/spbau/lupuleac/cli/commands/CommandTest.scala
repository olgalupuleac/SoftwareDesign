package ru.spbau.lupuleac.cli.commands

import java.io.{File, FileNotFoundException}
import java.util.regex.PatternSyntaxException

import org.scalatest.TryValues._
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

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
    val severalLinesFileLength =
      new File("src/test/resources/several_lines").length
    val totalFilesLength = 11 + severalLinesFileLength
    val expected =
      makeText("1 2 11 src/test/resources/a.txt",
               s"5 5 $severalLinesFileLength src/test/resources/several_lines",
               s"6 7 $totalFilesLength total")
    val outOr = WcCommand(
      List("src/test/resources/a.txt", "src/test/resources/several_lines"))(
      EmptyInput())
    outOr.success.value should be(expected)
  }

  "Wc command" should "calculate number of words if the line starts with space" in {
    val spacesFileLength = new File("src/test/resources/spaces.txt").length
    val outOr = WcCommand(List("src/test/resources/spaces.txt"))(EmptyInput())
    outOr.success.value should be(
      s"5 10 $spacesFileLength src/test/resources/spaces.txt")
  }

  "Wc command" should "print number of lines, words and bytes in file from stdin if no arguments are provided" in {
    val outOr = WcCommand(List())(InputWithText("aaa a"))
    outOr.success.value should be("1 2 5")
  }

  "Wc command" should "ignore stdin if the arguments are provided" in {
    val outOr =
      WcCommand(List("src/test/resources/a.txt"))(InputWithText("aaa a"))
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
    val outOr = CatCommand(
      List("src/test/resources/a.txt", "src/test/resources/several_lines"))(
      EmptyInput())
    outOr.success.value should be(
      makeText("hello world", "my", "lines", "in", "this", "file"))
  }

  "Cat command" should "ignore stdin if arguments are provided" in {
    val outOr =
      CatCommand(List("src/test/resources/several_lines"))(InputWithText("aa"))
    outOr.success.value should be(makeText("my", "lines", "in", "this", "file"))
  }

  "Process command" should "find a string in file" in {
    val command =
      if (System.getProperty("os.name").startsWith("Win"))
        "find \"i\" src/test/resources/several_lines"
      else "grep i src/test/resources/several_lines"
    val res = ProcessCommand(command, List())(EmptyInput())
    res.success.value should include(
      List("lines", "in", "this", "file").mkString(System.lineSeparator()))
    res.success.value should not include "my"
  }

  "Pwd command" should "print a current directory" in {
    val outOr = PwdCommand()(EmptyInput())
    outOr.success.value should endWith("SoftwareDesign")
  }

  "Cat command" should "print what exception occured if file doesn't exist" in {
    val outOr = CatCommand(
      List("src/test/resources/a.txt", "src/test/resources/not_a_file"))(
      EmptyInput())
    outOr.failure.exception shouldBe a[FileNotFoundException]
  }

  "Wc command" should "print what exception occured if file doesn't exist" in {
    val outOr = WcCommand(
      List("src/test/resources/a.txt", "src/test/resources/not_a_file"))(
      EmptyInput())
    outOr.failure.exception shouldBe a[FileNotFoundException]
  }

  "Grep config" should "evaluate arguments in this case correctly" in {
    val conf = GrepCommand.conf.parse("-i", "-A", "5", "[a]*", "file1", "file2")
    conf.success.value should be(
      GrepCommand.GrepConfig(ignoreCase = true,
                             wordRegex = false,
                             5,
                             "[a]*",
                             List("file1", "file2")))
  }

  "Grep config" should "evaluate arguments correctly if files are not provided" in {
    val conf = GrepCommand.conf.parse("-i", "-w", "[a]*")
    conf.success.value should be(
      GrepCommand
        .GrepConfig(ignoreCase = true, wordRegex = true, 0, "[a]*", List()))
  }

  "Grep config" should "evaluate arguments correctly if there is one file" in {
    val conf = GrepCommand.conf.parse("[a]*", "file1")
    conf.success.value should be(
      GrepCommand.GrepConfig(ignoreCase = false,
                             wordRegex = false,
                             0,
                             "[a]*",
                             List("file1")))
  }

  "Grep config" should "fail if no pattern is provided" in {
    val conf = GrepCommand.conf.parse("-w", "-A", "6")
    conf.failure.exception should have message "Missing required argument"
  }

  "Grep config" should "if number of lines after context is not specified" in {
    val conf = GrepCommand.conf.parse("-w", "-A", "fghj")
    conf.failure.exception shouldBe a[NumberFormatException]
  }

  "Grep command" should "find all usages of \"and\" ignoring case here" in {
    val expected = makeText(
      "I was alone and it was night",
      "And with endearing words so silent",
      "A million images and words, long since forgotten",
      "And as I wandered through the night",
      "And the present now",
      "The husband in the war who's never coming home",
      "... https://genius.com/Beardfish-and-the-stone-said-if-i-could-speak-lyrics"
    )
    val outOr = GrepCommand(
      List("-i", "AND", "src/test/resources/AndTheStoneSaid.txt"))(EmptyInput())
    outOr.success.value should be(expected)
  }

  "Grep command" should "find only lower case \"and\" words" in {
    val expected = makeText(
      "I was alone and it was night",
      "A million images and words, long since forgotten",
      "... https://genius.com/Beardfish-and-the-stone-said-if-i-could-speak-lyrics"
    )
    val outOr = GrepCommand(
      List("-w", "and", "src/test/resources/AndTheStoneSaid.txt"))(EmptyInput())
    outOr.success.value should be(expected)
  }

  "Grep command" should "print file names before lines if there are several files" in {
    val expected = makeText("src/test/resources/a.txt: hello world",
                            "src/test/resources/several_lines: lines",
                            "src/test/resources/several_lines: file")
    val outOr = GrepCommand(
      List("l",
           "src/test/resources/a.txt",
           "src/test/resources/several_lines"))(EmptyInput())
    outOr.success.value should be(expected)
  }

  "Grep command" should "print lines after context" in {
    val expected = makeText(
      "lines",
      "in",
      "this"
    )
    val outOr = GrepCommand(
      List("-A", "1", "in", "src/test/resources/several_lines"))(EmptyInput())
    outOr.success.value should be(expected)
  }

  "Grep command" should "fail if the number of lines is negative" in {
    val outOr = GrepCommand(
      List("-A", "-1", "in", "src/test/resources/several_lines"))(EmptyInput())
    outOr.failure.exception should have message ("Number of lines should not be a negative" +
      " number")
  }

  "Grep command" should "fail if the pattern is incorrect" in {
    val outOr = GrepCommand(List("[i*(n)", "src/test/resources/several_lines"))(
      EmptyInput())
    outOr.failure.exception shouldBe a[PatternSyntaxException]
  }
}
