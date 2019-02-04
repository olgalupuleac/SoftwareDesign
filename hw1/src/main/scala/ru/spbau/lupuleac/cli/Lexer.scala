package ru.spbau.lupuleac.cli
import scala.collection.mutable.ListBuffer


trait LexerAction {
  val token : String
  def processChar(c : Char) : (Boolean, LexerAction)
}

case class ReadingSingleQuoted(token: String) extends LexerAction {
  override def processChar(c : Char) : (Boolean, LexerAction) = {
    c match {
      case '\'' => (true, ReadingSimple(""))
      case _ => (false, ReadingSingleQuoted(token + c))
    }
  }
}


case class ReadingDoubleQuoted(token: String) extends LexerAction {
  override def processChar(c : Char) : (Boolean, LexerAction) = {
    c match {
      case '\"' => (true, ReadingSimple(""))
      case '$' => (false, ReadingVarCall(this, ""))
      case _ => (false, ReadingDoubleQuoted(token + c))
    }
  }
}

case class ReadingVarCall(parent : LexerAction, token : String) extends LexerAction {
  override def processChar(c : Char) : (Boolean, LexerAction) = {
    c match {
      case ' ' => (true, parent)
      case _ => (false, ReadingVarCall(parent, token + c))
    }
  }
}


case class ReadingSimple(token: String) extends LexerAction {
  override def processChar(c : Char) : (Boolean, LexerAction) = {
    c match {
      case ' ' => (true, ProcessToken(""))
      case '|' => (true, ProcessToken("|"))
      case '\'' => (true, ReadingSingleQuoted(""))
      case  '\"' => (true, ReadingDoubleQuoted(""))
      case '$' => (false, ReadingVarCall(this, ""))
      case _ => (false, ReadingSimple(token + c))
    }
  }
}

case class ProcessToken(token : String) extends LexerAction {
  override def processChar(c: Char): (Boolean, LexerAction) = (true, ReadingSimple(""))
}


class Lexer(scope : Scope) {
  def splitLineToTokens(line : String) : List[String] = {
    var res = new ListBuffer[String]()
    var action = ReadingSimple("") : LexerAction
    var curToken = ""
    for(c <- line) {
      val (addToToken, newAction) = action.processChar(c)
      if (addToToken) {
        val token = action match {
          case ReadingVarCall(_, varName) => scope(varName)
          case _ => action.token
        }
        curToken += token
      }
      newAction match {
        case ProcessToken("") =>
          res += curToken
          curToken = ""
          action = ReadingSimple("")
        case ProcessToken("|") =>
          res += curToken
          res += "|"
          curToken = ""
          action = ReadingSimple("")
        case _ => action = newAction
      }
    }
    res.toList
  }
}
