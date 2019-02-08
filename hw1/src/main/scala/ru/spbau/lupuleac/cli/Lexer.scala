package ru.spbau.lupuleac.cli

import scala.collection.mutable.ListBuffer


sealed trait Token

case class VarName(value: String) extends Token

case class Plain(value: String) extends Token

case class NotFinished() extends Token

case class Pipe() extends Token

trait LexerAction {
  val buffer: String

  def apply(c: Char): (Token, LexerAction)
}

case class SingleQuoted(buffer: String) extends LexerAction {
  override def apply(c: Char): (Token, LexerAction) = {
    c match {
      case '\'' => (Plain(buffer), Simple(""))
      case _ => (NotFinished(), SingleQuoted(buffer + c))
    }
  }
}


case class DoubleQuoted(buffer: String) extends LexerAction {
  override def apply(c: Char): (Token, LexerAction) = {
    c match {
      case '\"' => (Plain(buffer), Simple(""))
      case '$' => (Plain(buffer), VarCall(DoubleQuoted(""), ""))
      case _ => (NotFinished(), DoubleQuoted(buffer + c))
    }
  }
}

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

case class Terminate() extends LexerAction {
  override val buffer: String = ""

  override def apply(c: Char): (Token, LexerAction) = (Pipe(), Simple("")(c)._2)
}

class Lexer(scope: Scope) {
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
          splitByPipe += tokens.toList.mkString("").split("[\n]+")
          tokens.clear()
      }
    }
    splitByPipe.toList
  }
}
