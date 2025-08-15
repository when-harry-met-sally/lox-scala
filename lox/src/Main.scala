package lox
import scanner.Scanner
import token.Token

object Lox {
  def scanText(text: String): List[Token] = {
    val tokens = Scanner.scan(text)
    tokens.foreach(println)
    tokens
  }

  def main(args: Array[String]): Unit = {
    // Example usage
    scanText("Scala is pretty snazzy")
  }
}
