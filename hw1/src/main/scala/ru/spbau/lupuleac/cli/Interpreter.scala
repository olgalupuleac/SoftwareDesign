package ru.spbau.lupuleac.cli

import java.nio.file.{Path, Paths}

import ru.spbau.lupuleac.cli.commands.{CommandFactory, EmptyInput, Input, Stdin}

/**
  * Class, which takes line one by one and returns the output for each command.
  **/
class Interpreter {
  private val scope = new Scope()

  var currentDirectory : Path = Paths.get(System.getProperty("user.dir"))

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
    val lexer = new Parser(scope)
    val listsOfTokens = lexer.splitLineToTokens(line)
    var input = EmptyInput(): Input
    try {
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
          val out = command(this)
          input = Stdin(out)
          args.clear()
        }
      }
    } catch (e: Exception) {
      return e.getMessage
    }
    input.text
  }
}