package ru.spbau.lupuleac.cli

/**
  * Represents a scope with environment variables.
  */
class Scope {
  private val map = collection.mutable.Map[String, String]()

  /**
    * Takes a value from scope
    *
    * @param varName is a variable name
    * @return variable value if it is in scope or empty string otherwise
    */
  def apply(varName: String): String = {
    map getOrElse(varName, "")
  }

  /**
    * Sets an environment variable.
    *
    * @param name  is a variable name
    * @param value is a variable value
    */
  def apply(name: String, value: String): Unit = {
    map(name) = value
  }
}
