package ru.spbau.lupuleac.cli


class Interpreter {
  private val scope = new Scope()
  private val args = collection.mutable.MutableList[Argument]()

  def processAssignment(token : String) : Boolean = {
    if (!(token matches ".*=.*")) {
      return false
    }
    val leftAndRight = token.split("=", 2)
    assert(leftAndRight.length == 2)
    scope(leftAndRight.head, leftAndRight(1))
    true
  }


  def apply(line : String): Output = {
    val lexer = new Lexer(scope)
    val listsOfTokens = lexer.splitLineToTokens(line)
    var res = new Output("")
    for (tokens <- listsOfTokens) {
      var assignment = true
      var command = null : Command
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
        res = command(args:_*)
        args.clear()
        args += res
      }
    }
    res
  }

}