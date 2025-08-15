package lox
import scanner.Scanner
import parser.Parser
import token.Token

object Lox {
  def logText(text: String)= {
    println("---INPUT TEXT---")
    println(text)
    println("")
  }

  def logTokens(tokens: List[Token])= {
    println("-----TOKENS-----")
    tokens.foreach(println)
    println("")
  }

  def logParsTree(tokens: List[Token])= {
    println("-----TOKENS-----")
    tokens.foreach(println)
    println("")
  }


  def main(args: Array[String]): Unit = {
    // Example usage
    val text = "1.23"
    logText(text)

    val tokens = Scanner.scan(text)
    logTokens(tokens)

    val parsed = Parser.parse(tokens)
  }
}
