package ru.spbau.lupuleac.cli

import ru.spbau.lupuleac.cli.commands._

import scala.util.{Failure, Success, Try}

/**
  * Class, which takes line one by one and returns the output for each command.
  **/
class Interpreter {
  private val scope = new Scope()

  /**
    * Parses assignments, saves them in scope and return the rest tokens
    *
    * @param tokens is a tokens to be parsed
    * @return the tokens which should not be interpreted as assignments
    *         (and should be interpreted as command and it's arguments)
    */
  def processAssignment(tokens: Seq[String]): Seq[String] = {
    if (tokens.isEmpty || !(tokens.head matches ".*=.*")) {
      return tokens
    }
    val leftAndRight = tokens.head.split("=", 2)
    assert(leftAndRight.length == 2)
    scope(leftAndRight.head, leftAndRight(1))
    processAssignment(tokens.tail)
  }

  /**
    * Executes the commands one by one.
    *
    * @param commands are commands to be executed
    * @param input    is an input from the previous command
    * @return a Success with the last input if no exceptions occured,
    *         or Failure with exception otherwise
    */
  def processCommands(commands: Seq[Command], input: Input): Try[String] = {
    if (commands.isEmpty) {
      return Try(input.get)
    }
    val res = commands.head(input)
    res match {
      case Failure(_) => res
      case Success(s) => processCommands(commands.tail, InputWithText(s))
    }
  }

  /**
    * Executes the given line.
    *
    * @param line is a line to be executed
    * @return the output, which should be printed
    */
  def apply(line: String): String = {
    val parser = new Parser(scope)
    val listsOfTokensOr = parser.splitLineToTokens(line)
    listsOfTokensOr match {
      case Failure(s) => s.getMessage
      case Success(listsOfTokens) =>
        val tokensWithoutAssignments =
          for (token <- listsOfTokens)
            yield processAssignment(token)

        val commands =
          tokensWithoutAssignments
            .filter(s => s.nonEmpty)
            .map(x => CommandFactory(x.head, x.tail))
        processCommands(commands, EmptyInput()) match {
          case Failure(x) => x.getMessage
          case Success(x) => x
        }
    }
  }

}
