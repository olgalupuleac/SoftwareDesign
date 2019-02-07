package ru.spbau.lupuleac.cli

class Scope {
  private val map = collection.mutable.Map[String, String]()

  def apply(varName: String): String = {
    map getOrElse(varName, "")
  }

  def apply(name: String, value: String): Unit = {
    map(name) = value
  }
}
