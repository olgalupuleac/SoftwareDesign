package ru.spbau.lupuleac.cli.commands

/**
  * Returns a file (or files) contents.
  */
case class CatCommand(stdin: Option[String], arguments: List[String]) extends Command {
  override val name: String = "cat"

  override def validate(): Boolean = !(arguments.isEmpty && stdin.isEmpty)

  override def execute(): String = {
    if (arguments.isEmpty) {
      return stdin.get
    }
    arguments.map(x => File(x)).mkString("\n")
  }
}
