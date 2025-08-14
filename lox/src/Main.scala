package lox
import scanner.Scanner

object Example {
  def main(args: Array[String]): Unit = {
    val text = "Scala is pretty snazzy"
    val tokens = Scanner.scan("a")
    tokens.foreach(println)
  }
}
