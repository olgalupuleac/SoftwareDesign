package ru.spbau.lupuleac.cli

import scala.collection.mutable.ListBuffer

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
  * Represents a state in finite-state machine used for parsing a string.
  */
trait LexerAction {
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
  def apply(c: Char): (Token, LexerAction)
}

/**
  *
  * @param buffer
  */
case class SingleQuoted(buffer: String) extends LexerAction {
  override def apply(c: Char): (Token, LexerAction) = {
    c match {
      case '\'' => (Plain(buffer), Simple(""))
      case _ => (NotFinished(), SingleQuoted(buffer + c))
    }
  }
}

/**
  *
  * @param buffer
  */
case class DoubleQuoted(buffer: String) extends LexerAction {
  override def apply(c: Char): (Token, LexerAction) = {
    c match {
      case '\"' => (Plain(buffer), Simple(""))
      case '$' => (Plain(buffer), VarCall(DoubleQuoted(""), ""))
      case _ => (NotFinished(), DoubleQuoted(buffer + c))
    }
  }
}

/**
  *
  * @param parent
  * @param buffer
  */
case class VarCall(parent: LexerAction, buffer: String) extends LexerAction {
  override def apply(c: Char): (Token, LexerAction) = {
    val stopRegex = "(\\s|\"|\'|\\|)"
    if (c.toString matches stopRegex) {
      (VarName(buffer), parent(c)._2)
    } else {
      (NotFinished(), VarCall(parent, buffer + c))
    }
  }
}

/**
  *
  * @param buffer
  */
case class Simple(buffer: String) extends LexerAction {
  override def apply(c: Char): (Token, LexerAction) = {
    c match {
      case ' ' => (NotFinished(), Simple(buffer + "\n"))
      case '|' => (Plain(buffer + "\n"), Terminate())
      case '\'' => (Plain(buffer), SingleQuoted(""))
      case '\"' => (Plain(buffer), DoubleQuoted(""))
      case '$' => (Plain(buffer), VarCall(Simple(""), ""))
      case _ => (NotFinished(), Simple(buffer + c))
    }
  }
}

/**
  *
  */
case class Terminate() extends LexerAction {
  override val buffer: String = ""

  override def apply(c: Char): (Token, LexerAction) = (Pipe(), Simple("")(c)._2)
}

/**
  *
  * @param scope
  */
class Lexer(scope: Scope) {
  /**
    * 
    * @param line
    * @return
    */
  def splitLineToTokens(line: String): List[Array[String]] = {
    var action = Simple(""): LexerAction
    var splitByPipe = ListBuffer[Array[String]]()
    var tokens = ListBuffer[String]()
    for (c <- line + "| ") {
      val (token, newAction) = action(c)
      action = newAction
      token match {
        case NotFinished() =>
        case VarName(name) => tokens += scope(name)
        case Plain(text) => tokens += text
        case Pipe() =>
          splitByPipe += tokens.toList.mkString("").split("[\n]+").filter(x => !(x matches "(\n)*"))
          tokens.clear()
      }
    }
    splitByPipe.toList
  }
}
