package bar
import utest.*
import scanner.Scanner
import token.{Token, TokenType}

object BarTests extends TestSuite {
  def tests = Tests {
    test("Scans welformed decimals") {
      val tokens = Scanner.scan("1.23")
      val expected = Token(TokenType.NUMBER, "1.23", Some("1.23"), 1)
      assert(tokens.head == expected)
    }
    test("Scans integers") {
      val tokens = Scanner.scan("1")
      val expected = Token(TokenType.NUMBER, "1", Some("1"), 1)
      assert(tokens.head == expected)
    }
    test("Scans numbers with multiple decimals as invalid") {
      val tokens = Scanner.scan("1.2.")
      val expected = Token(TokenType.UNKNOWN, "1.2.", Some("1.2."), 1)
      assert(tokens.head == expected)
    }
    test("Scans keyword") {
      val tokens = Scanner.scan("while")
      val expected = Token(TokenType.WHILE, "while", Some("while"), 1)
      assert(tokens.head == expected)
    }
    test("Scans identifier") {
      val tokens = Scanner.scan("bob")
      val expected = Token(TokenType.IDENTIFIER, "bob", Some("bob"), 1)
      assert(tokens.head == expected)
    }
    test("Scans quote") {
      val tokens = Scanner.scan("\"bob\"")
      println(tokens.head)
      val expected = Token(TokenType.STRING, "bob", Some("bob"), 1)
      assert(tokens.head == expected)
    }
  }
}
