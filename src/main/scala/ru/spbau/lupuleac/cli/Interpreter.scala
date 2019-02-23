package ru.spbau.lupuleac.cli

import ru.spbau.lupuleac.cli.commands._

import scala.util.{Failure, Success}

/**
  * Class, which takes line one by one and returns the output for each command.
  **/
class Interpreter {
  private val scope = new Scope()

  /**
    * Checks if the token can be parsed as an assignment (e.g. x=1). If true, parses it and saves the result in scope.
    *
    * @param token is a token to be parsed
    * @return true if token can be parsed as an assignment
    */
  def processAssignment(token: String): Boolean = {
    if (!(token matches ".*=.*")) {
      return false
    }
    val leftAndRight = token.split("=", 2)
    assert(leftAndRight.length == 2)
    scope(leftAndRight.head, leftAndRight(1))
    true
  }

  /**
    * Executes the given line.
    *
    * @param line is a line to be executed
    * @return the output, which should be printed
    */
  def apply(line: String): String = {
    val parser = new Parser(scope)
    val listsOfTokens = parser.splitLineToTokens(line)
    var input = EmptyInput(): Input
    for (tokens <- listsOfTokens) {
      var assignment = true
      var commandName = None: Option[String]
      val args = collection.mutable.ListBuffer[String]()
      for (token <- tokens) {
        if (assignment) {
          assignment = processAssignment(token)
          if (!assignment) {
            commandName = Some(token)
          }
        } else {
          args += token
        }
      }
      if (commandName.isDefined) {
        val command = CommandFactory(commandName.get, input, args.toList)
        val outOr = command()
        outOr match {
          case Success(out) =>
            input = InputWithText(out)
            args.clear()
          case Failure(s) => return "bash: " + command.name + ": " + s
        }
      }
    }
    input.text
  }
}