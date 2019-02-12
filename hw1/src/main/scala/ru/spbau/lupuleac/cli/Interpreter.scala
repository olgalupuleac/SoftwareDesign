package ru.spbau.lupuleac.cli

/**
  * Class, which takes line one by one and returns the output for each command.
  **/
class Interpreter {
  private val scope = new Scope()
  private val args = collection.mutable.MutableList[Argument]()

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
    val lexer = new Lexer(scope)
    val listsOfTokens = lexer.splitLineToTokens(line)
    var res = Output("")
    for (tokens <- listsOfTokens) {
      var assignment = true
      var command = null: Command
      for (token <- tokens) {
        if (assignment) {
          assignment = processAssignment(token)
          if (!assignment) {
            command = Command(token)
          }
        } else {
          args += CommandLineArgument(token)
        }
      }
      if (command != null) {
        res = command(args: _*)
        args.clear()
        args += res
      }
    }
    res.asFile
  }
}