package token
import scala.collection.immutable.HashMap
case class Token(
  tokenType: TokenType,
  lexeme: String,
  literal: Option[Any],
  line: Int,
)

object Keywords {
  val keywordTokenTypeMap = HashMap(
  "and"    -> TokenType.AND,
  "class"  -> TokenType.CLASS,
  "else"   -> TokenType.ELSE,
  "false"  -> TokenType.FALSE,
  "fun"    -> TokenType.FUN,
  "for"    -> TokenType.FOR,
  "if"     -> TokenType.IF,
  "nil"    -> TokenType.NIL,
  "or"     -> TokenType.OR,
  "print"  -> TokenType.PRINT,
  "return" -> TokenType.RETURN,
  "super"  -> TokenType.SUPER,
  "this"   -> TokenType.THIS,
  "true"   -> TokenType.TRUE,
  "var"    -> TokenType.VAR,
  "while"  -> TokenType.WHILE
)
}

enum TokenType {
    // SINGLE OR MULTI CHAR TOKEN
    case     LEFT_PAREN
    case     RIGHT_PAREN
    case     LEFT_BRACE
    case     RIGHT_BRACE
    case     COMMA
    case     DOT
    case     MINUS
    case     PLUS
    case     SEMICOLON
    case     SLASH
    case     STAR
    // SINGLE OR MULTI CHAR TOKEN
    case     BANG
    case     BANG_EQUAL
    case     EQUAL
    case     EQUAL_EQUAL
    case     GREATER
    case     GREATER_EQUAL
    case     LESS
    case     LESS_EQUAL
        // LITERALS
    case     IDENTIFIER
    case     STRING
    case     NUMBER
        // KEYWORDS
    case     AND
    case     CLASS
    case     ELSE
    case     FALSE
    case     FUN
    case     FOR
    case     IF
    case     NIL
    case     OR
    case     PRINT
    case     RETURN
    case     SUPER
    case     THIS
    case     TRUE
    case     VAR
    case     WHILE
    case     EOF
    case     UNKNOWN
}
