package ru.spbau.lupuleac.cli
import org.scalatest._

class LexerTest extends FlatSpec with Matchers {
  "A Parser" should "split to words a line without quotes" in {
    val line = "Hello new    world"
    val parser = new Lexer(new Scope())
    val words = parser.splitLineToTokens(line)
    words.head should be ("Hello")
    words(1) should be ("new")
    words(2) should be ("world")
  }

  "A parser" should "ignore the arguments in quotes" in {
    val line = "Hello 'new k'  \" my name \" world"
    val parser = new Lexer(new Scope())
    val tokens = parser.splitLineToTokens(line)
    tokens.head should be ("Hello")
    tokens(1) should be ("new k")
    tokens(2) should be (" my name ")
    tokens(3) should be ("world")
  }

  "A parser" should "also ignore quotes inside quotes" in {
    val line = "Hello 'new k  \" my name \" world'"
    val parser = new Lexer(new Scope())
    val tokens = parser.splitLineToTokens(line)
    tokens.head should be ("Hello")
    tokens(1) should be ("new k  \" my name \" world")
  }


}
