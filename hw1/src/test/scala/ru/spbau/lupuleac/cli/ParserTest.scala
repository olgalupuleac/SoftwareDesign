package ru.spbau.lupuleac.cli

import org.scalatest._

class ParserTest extends FlatSpec with Matchers {
  "Parser" should "split to words a line without quotes" in {
    val line = "Hello new    world"
    val parser = new Parser(new Scope())
    val tokens = parser.splitLineToTokens(line)
    tokens.length should be (1)
    tokens.head should be(List("Hello", "new", "world"))
  }

  "Parser" should "ignore the arguments in quotes" in {
    val line = "Hello 'new k'  \" my name \" world"
    val parser = new Parser(new Scope())
    val tokens = parser.splitLineToTokens(line)
    tokens.length should be(1)
    tokens.head should be (List("Hello", "new k", " my name ", "world"))
  }

  "Parser" should "also ignore quotes inside quotes" in {
    val line = "Hello 'new k  \" my name \" world'"
    val parser = new Parser(new Scope())
    val tokens = parser.splitLineToTokens(line)
    tokens.length should be(1)
    tokens.head should be(List("Hello", "new k  \" my name \" world"))
  }

  "Parser" should "also parse several quoted tokens as one" in {
    val line = "e'c'\"h\"o"
    val parser = new Parser(new Scope())
    val tokens = parser.splitLineToTokens(line)
    tokens.length should be (1)
    tokens.head should be(List("echo"))
  }

  "Parser" should "substitute variables correctly" in {
    val line = "echo \"$FILE x\""
    val scope = new Scope()
    scope("FILE", "au")
    val parser = new Parser(scope)
    val tokens = parser.splitLineToTokens(line)
    tokens.length should be (1)
    tokens.head should be(List("echo", "au x"))
  }

  "Parser " should "substitue variables correctly even in this case" in {
    val line = "echo hello$x|c"
    val scope = new Scope()
    scope("x", "1")
    val parser = new Parser(scope)
    val tokens = parser.splitLineToTokens(line)
    tokens.length should be (2)
    tokens.head should be (List("echo", "hello1"))
    tokens(1) should be (List("c"))
  }

  "Parser" should "split string by pipes" in {
    val scope = new Scope()
    val parser = new Parser(scope)
    val tokens = parser.splitLineToTokens("echo au | wc")
    tokens.length should be (2)
    tokens.head should be (List("echo", "au"))
    tokens(1) should be (List("wc"))
  }

  "Parser" should "substitute several variables" in {
    val scope = new Scope()
    scope("a", "b")
    scope("x", "y")
    val parser = new Parser(scope)
    val tokens = parser.splitLineToTokens("$a$x")
    tokens.head should be(List("by"))
  }

  "Parser" should "interpreted several tokens without space between as one" in {
    val scope = new Scope()
    val parser = new Parser(scope)
    val tokens = parser.splitLineToTokens("\"e  \"\"  t y\"")
    tokens.length should be (1)
    tokens.head should be (List("e    t y"))
  }

  "Parser" should "throw an exception if the quote isn't closed" in {
    val scope = new Scope()
    val parser = new Parser(scope)
    an [IllegalArgumentException] should be thrownBy parser.splitLineToTokens("\' hj- | ght")
  }

  "Parser" should "throw an exception if the pipe is followed by another one" in {
    val scope = new Scope()
    val parser = new Parser(scope)
    an [IllegalArgumentException] should be thrownBy parser.splitLineToTokens("echo | |")
  }
}
