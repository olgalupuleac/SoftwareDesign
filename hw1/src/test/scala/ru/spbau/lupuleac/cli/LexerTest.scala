package ru.spbau.lupuleac.cli
import org.scalatest._

class LexerTest extends FlatSpec with Matchers {
  "A lexer" should "split to words a line without quotes" in {
    val line = "Hello new    world"
    val parser = new Lexer(new Scope())
    val splitByPipe = parser.splitLineToTokens(line)
    val words = splitByPipe.head
    words.head should be ("Hello")
    words(1) should be ("new")
    words(2) should be ("world")
  }

  "A lexer" should "ignore the arguments in quotes" in {
    val line = "Hello 'new k'  \" my name \" world"
    val parser = new Lexer(new Scope())
    val tokens = parser.splitLineToTokens(line).head
    tokens.length should be (4)
    tokens.head should be ("Hello")
    tokens(1) should be ("new k")
    tokens(2) should be (" my name ")
    tokens(3) should be ("world")
  }

  "A lexer" should "also ignore quotes inside quotes" in {
    val line = "Hello 'new k  \" my name \" world'"
    val parser = new Lexer(new Scope())
    val tokens = parser.splitLineToTokens(line).head
    tokens.length should be (2)
    tokens.head should be ("Hello")
    tokens(1) should be ("new k  \" my name \" world")
  }

  "A lexer" should "also parse several quoted tokens as one" in {
    val line = "e'c'\"h\"o"
    val parser = new Lexer(new Scope())
    val tokens = parser.splitLineToTokens(line).head
    tokens.head should be ("echo")
    tokens.length should be(1)
  }

  "A lexer" should "substitute variables correctly" in {
    val line = "echo \"$FILE x\""
    val scope = new Scope()
    scope("FILE", "au")
    val parser = new Lexer(scope)
    val tokens = parser.splitLineToTokens(line).head
    tokens.head should be ("echo")
    tokens(1) should be ("au x")
    tokens.length should be(2)
  }
}
