package ru.spbau.lupuleac.cli

import scala.util.Try

sealed trait Token

/**
  * Represents a variable call
  *
  * @param name is a variable name
  */
case class VarName(name: String) extends Token

/**
  * Represents a plain text
  *
  * @param text is a text
  */
case class Plain(text: String) extends Token

/**
  * Represents token which will be ignored when all tokens will be joined
  */
case class NotFinished() extends Token

/**
  * Represents pipe token by which the result will be split
  */
case class Pipe() extends Token

/**
  * Exception which is thrown if the sring cannot be parsed.
  *
  * @param s is a message
  */
class ParseException(s: String) extends Exception(s) {}

/**
  * Represents a state in finite-state machine used for parsing a string.
  */
sealed trait ParserAction {

  /**
    * Current string which we had during parsing.
    * New string is created when token is finished
    */
  val buffer: String

  /**
    * Takes the next character of a line and returns the token which corresponds to this state and the next state.
    *
    * @param c is a char
    * @return token and next state
    */
  def apply(c: Char): (Token, ParserAction)
}

/**
  * Special characters
  */
object ParserAction {
  val space = ' '
  val singleQuote = '\''
  val doubleQuote = '\"'
  val pipe = '|'
  val envVar = '$'
  val terminate = '\u0000'
}

/**
  * Reading a part of the line in single quotes
  *
  * @param buffer is a string to which represents a current value of token
  */
case class SingleQuoted(buffer: String) extends ParserAction {
  override def apply(c: Char): (Token, ParserAction) = {
    c match {
      case ParserAction.singleQuote => (Plain(buffer), Simple(""))
      case ParserAction.terminate =>
        throw new ParseException("Syntax error : unclosed quote")
      case _ => (NotFinished(), SingleQuoted(buffer + c))
    }
  }
}

/**
  * Reading a part of the line in double quotes
  *
  * @param buffer is a string to which represents a current value of token
  */
case class DoubleQuoted(buffer: String) extends ParserAction {
  override def apply(c: Char): (Token, ParserAction) = {
    c match {
      case ParserAction.doubleQuote => (Plain(buffer), Simple(""))
      case ParserAction.envVar      => (Plain(buffer), VarCall(DoubleQuoted(""), ""))
      case ParserAction.terminate =>
        throw new ParseException("Syntax error : unclosed quote")
      case _ => (NotFinished(), DoubleQuoted(buffer + c))
    }
  }
}

/**
  * Reading an environment variable name.
  *
  * @param parent is a mode which was before the beginning of the name (it could be DoubleQuoted or Simple)
  * @param buffer is a string to which represents a current value of token
  */
case class VarCall(parent: ParserAction, buffer: String) extends ParserAction {
  override def apply(c: Char): (Token, ParserAction) = {
    c match {
      case ParserAction.envVar      => (VarName(buffer), VarCall(parent, ""))
      case ParserAction.space       => (VarName(buffer), parent(c)._2)
      case ParserAction.doubleQuote => (VarName(buffer), parent(c)._2)
      case ParserAction.singleQuote =>
        throw new ParseException(
          "Syntax error : variable name cannot contain quotes")
      case ParserAction.pipe      => (VarName(buffer), parent(c)._2)
      case ParserAction.terminate => (VarName(buffer), parent(c)._2)
      case _                      => (NotFinished(), VarCall(parent, buffer + c))
    }
  }
}

/**
  * Reading a part of the line without quotes
  *
  * @param buffer is a string to which represents a current value of token
  */
case class Simple(buffer: String) extends ParserAction {
  override def apply(c: Char): (Token, ParserAction) = {
    c match {
      case ParserAction.space =>
        (NotFinished(), Simple(buffer + System.lineSeparator()))
      case ParserAction.pipe        => (Plain(buffer), PipeGap())
      case ParserAction.singleQuote => (Plain(buffer), SingleQuoted(""))
      case ParserAction.doubleQuote => (Plain(buffer), DoubleQuoted(""))
      case ParserAction.envVar      => (Plain(buffer), VarCall(Simple(""), ""))
      case ParserAction.terminate   => (Plain(buffer), null)
      case _                        => (NotFinished(), Simple(buffer + c))
    }
  }
}

/**
  * It goes after pipe.
  */
case class PipeGap() extends ParserAction {
  override val buffer: String = ""

  /**
    * Consuming a character it returns a pipe and a simple state which consumed this character.
    *
    * @param c is a char
    * @return token and next state
    */
  override def apply(c: Char): (Token, ParserAction) = c match {
    case ParserAction.space => (NotFinished(), PipeGap())
    case ParserAction.terminate =>
      throw new ParseException(
        "Syntax error : unclosed pipe in the end of the line")
    case ParserAction.pipe =>
      throw new ParseException("Syntax error: empty space between pipes")
    case _ => (Pipe(), Simple("")(c)._2)
  }
}

/**
  * Class which takes a string and splits it by tokens.
  *
  * @param scope is a scope from which an environment variables will be taken.
  */
class Parser(scope: Scope) {
  def getTokens(action: ParserAction, chars: Seq[Char]): List[Token] = {
    if (chars.isEmpty) {
      return List()
    }
    val (token, newAction) = action(chars.head)
    token :: getTokens(newAction, chars.tail)
  }

  /**
    *
    * @param line is a line to be split
    * @return list of arrays of token, in the outer list elements are split by pipes.
    */
  def splitLineToTokens(line: String): Try[Array[Array[String]]] = {
    Try {
      val tokens =
        getTokens(Simple(""), (line + ParserAction.terminate).toCharArray)
      val splitByPipes = tokens
        .map {
          case t @ Pipe()    => ParserAction.terminate
          case VarName(name) => scope(name)
          case Plain(text)   => text
          case _             => ""
        }
        .mkString("")
        .split(ParserAction.terminate)
      val regexToSplit = "[" + System.lineSeparator() + "]+"
      splitByPipes.map(x => x.split(regexToSplit).filter(x => x.nonEmpty))
    }
  }
}
