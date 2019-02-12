package ru.spbau.lupuleac.cli

import org.scalatest._

class LexerTest extends FlatSpec with Matchers {
  "A lexer" should "split to words a line without quotes" in {
    val line = "Hello new    world"
    val parser = new Lexer(new Scope())
    val tokens = parser.splitLineToTokens(line)
    tokens.length should be (1)
    tokens.head should be(List("Hello", "new", "world"))
  }

  "A lexer" should "ignore the arguments in quotes" in {
    val line = "Hello 'new k'  \" my name \" world"
    val parser = new Lexer(new Scope())
    val tokens = parser.splitLineToTokens(line)
    tokens.length should be(1)
    tokens.head should be (List("Hello", "new k", " my name ", "world"))
  }

  "A lexer" should "also ignore quotes inside quotes" in {
    val line = "Hello 'new k  \" my name \" world'"
    val parser = new Lexer(new Scope())
    val tokens = parser.splitLineToTokens(line)
    tokens.length should be(1)
    tokens.head should be(List("Hello", "new k  \" my name \" world"))
  }

  "A lexer" should "also parse several quoted tokens as one" in {
    val line = "e'c'\"h\"o"
    val parser = new Lexer(new Scope())
    val tokens = parser.splitLineToTokens(line)
    tokens.length should be (1)
    tokens.head should be(List("echo"))
  }

  "A lexer" should "substitute variables correctly" in {
    val line = "echo \"$FILE x\""
    val scope = new Scope()
    scope("FILE", "au")
    val parser = new Lexer(scope)
    val tokens = parser.splitLineToTokens(line)
    tokens.length should be (1)
    tokens.head should be(List("echo", "au x"))
  }

  "A lexer " should "substitue variables correctly even in this case" in {
    val line = "echo hello$x|c"
    val scope = new Scope()
    scope("x", "1")
    val lexer = new Lexer(scope)
    val tokens = lexer.splitLineToTokens(line)
    tokens.length should be (2)
    tokens.head should be (List("echo", "hello1"))
    tokens(1) should be (List("c"))
  }

  "A lexer" should "split string by pipes" in {
    val scope = new Scope()
    val lexer = new Lexer(scope)
    val tokens = lexer.splitLineToTokens("echo au | wc")
    tokens.head.foreach(x => println(x))
    tokens(1).foreach(x => println(x))
  }
}
