package ru.spbau.lupuleac.cli

import org.scalatest.TryValues._
import org.scalatest._

class ParserTest extends FlatSpec with Matchers {
  "Parser" should "split to words a line without quotes" in {
    val line = "Hello new    world"
    val parser = new Parser(new Scope())
    val tokensOr = parser.splitLineToTokens(line)
    tokensOr.success.value.length should be(1)
    tokensOr.success.value.head should be(Array("Hello", "new", "world"))
  }

  "Parser" should "ignore the arguments in quotes" in {
    val line = "Hello 'new k'  \" my name \" world"
    val parser = new Parser(new Scope())
    val tokensOr = parser.splitLineToTokens(line)
    tokensOr.success.value.length should be(1)
    tokensOr.success.value.head should be(
      Array("Hello", "new k", " my name ", "world"))
  }

  "Parser" should "also ignore quotes inside quotes" in {
    val line = "Hello 'new k  \" my name \" world'"
    val parser = new Parser(new Scope())
    val tokensOr = parser.splitLineToTokens(line)
    tokensOr.success.value.length should be(1)
    tokensOr.success.value.head should be(
      Array("Hello", "new k  \" my name \" world"))
  }

  "Parser" should "also parse several quoted tokens as one" in {
    val line = "e'c'\"h\"o"
    val parser = new Parser(new Scope())
    val tokensOr = parser.splitLineToTokens(line)
    tokensOr.success.value.length should be(1)
    tokensOr.success.value.head should be(Array("echo"))
  }

  "Parser" should "substitute variables correctly" in {
    val line = "echo \"$FILE x\""
    val scope = new Scope()
    scope("FILE", "au")
    val parser = new Parser(scope)
    val tokensOr = parser.splitLineToTokens(line)
    tokensOr.success.value.length should be(1)
    tokensOr.success.value.head should be(Array("echo", "au x"))
  }

  "Parser " should "substitue variables correctly even in this case" in {
    val line = "echo hello$x|c"
    val scope = new Scope()
    scope("x", "1")
    val parser = new Parser(scope)
    val tokensOr = parser.splitLineToTokens(line)
    tokensOr.success.value.length should be(2)
    tokensOr.success.value.head should be(Array("echo", "hello1"))
    tokensOr.success.value(1) should be(Array("c"))
  }

  "Parser" should "split string by pipes" in {
    val scope = new Scope()
    val parser = new Parser(scope)
    val tokensOr = parser.splitLineToTokens("echo au | wc")
    tokensOr.success.value.length should be(2)
    tokensOr.success.value.head should be(Array("echo", "au"))
    tokensOr.success.value(1) should be(Array("wc"))
  }

  "Parser" should "substitute several variables" in {
    val scope = new Scope()
    scope("a", "b")
    scope("x", "y")
    val parser = new Parser(scope)
    val tokensOr = parser.splitLineToTokens("$a$x")
    tokensOr.success.value.head should be(Array("by"))
  }

  "Parser" should "interpreted several tokens without space between as one" in {
    val scope = new Scope()
    val parser = new Parser(scope)
    val tokensOr = parser.splitLineToTokens("\"e  \"\"  t y\"")
    tokensOr.success.value.length should be(1)
    tokensOr.success.value.head should be(Array("e    t y"))
  }

  "Parser" should "throw an exception if the quote isn't closed" in {
    val scope = new Scope()
    val parser = new Parser(scope)
    val resOr = parser.splitLineToTokens("\' hj- | ght")
    resOr.failure.exception.getMessage should be(
      "Syntax error : unclosed quote")
  }

  "Parser" should "throw an exception if the pipe is followed by another one" in {
    val scope = new Scope()
    val parser = new Parser(scope)
    val resOr = parser.splitLineToTokens("echo | |")
    resOr.failure.exception.getMessage should be(
      "Syntax error: empty space between pipes")
  }
}
