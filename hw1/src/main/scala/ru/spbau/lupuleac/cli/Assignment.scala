package ru.spbau.lupuleac.cli

class Assignment(name : String, value : String) extends Expression

object Assignment {
  def apply(name: String, value: String): Assignment = new Assignment(name, value)
}