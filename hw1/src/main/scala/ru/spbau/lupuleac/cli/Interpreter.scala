package ru.spbau.lupuleac.cli


trait InterpreterAction {
  def nextAction(str : String) : InterpreterAction
}

case object ExecuteAction extends InterpreterAction {
  override def nextAction(str: String): InterpreterAction = if (str contains "=") {
    AssignmentAction
  } else {
    CommandAction
  }
}

case object AssignmentAction extends InterpreterAction {
  override def nextAction(str: String): InterpreterAction = if (str contains "=") {
    AssignmentAction
  } else {
    if (str == "|") {
      ExecuteAction
    }
    CommandAction
  }
}

case object CommandAction extends InterpreterAction {
  override def nextAction(str: String): InterpreterAction = if(str == "|") {
    ExecuteAction
  } else {
    ArgumentAction
  }
}

case object ArgumentAction extends InterpreterAction {
  override def nextAction(str: String): InterpreterAction = if(str == "|") {
    ExecuteAction
  } else {
    ArgumentAction
  }
}


class Interpreter {
  private val scope = new Scope()
  private var curCommand = null : Command
  private val args = collection.mutable.MutableList[Argument]()

  def processLine(line : String): Unit = {
    val lexer = new Lexer(scope)
    val tokens = lexer.splitLineToTokens(line)
    if (tokens.isEmpty) {
      return
    }
    var action = ExecuteAction.nextAction(tokens.head)
    for (token <- tokens) {
      apply(action, token)
      action.nextAction(token)
    }
  }

  def apply(action: InterpreterAction, str : String) {
    action match {
      case AssignmentAction =>
        val leftAndRight = str.split("=")
        if (leftAndRight.length > 2) {
          throw new RuntimeException("Cannot parse")
        }
        scope(leftAndRight(0), leftAndRight(1))
      case CommandAction => curCommand = Command(str)
      case ArgumentAction => args += CommandLineArgument(str)
      case ExecuteAction =>
        val output = curCommand.execute(args:_*)
        curCommand = null
        args.clear()
        args += output
    }
  }
}
