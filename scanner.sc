//> using file "./token.sc"
import token.{Token,TokenType,Keywords}

@annotation.tailrec
def peek_quote(chars: List[Char], literal: List[Char]): (List[Char], List[Char]) = chars match {
  case Nil => (Nil, literal.reverse)
  case '"'::rest => (rest, literal.reverse)
  case char::rest=> peek_quote(rest, char::literal)
}

@annotation.tailrec
def peek_alphabetical(chars: List[Char], literal: List[Char]): (List[Char], List[Char]) = chars match {
  case Nil => (Nil, literal.reverse)
  case (' ' | '\r' | '\t') ::rest => (rest, literal.reverse)
  case char::rest if char.isLetter || char.isDigit || char == '_' => (rest, literal.reverse)
  case char::rest => peek_alphabetical(rest, char::literal)
}

@annotation.tailrec
// TODO: Revisit, this sould probably return option type
def peek_numeric(chars: List[Char], literal: List[Char]): (List[Char], List[Char]) = chars match {
  case Nil => (Nil, literal.reverse)
  case (' ' | '\r' | '\t') ::rest => (rest, literal.reverse)
  case char::rest => peek_numeric(rest, char::literal)
}

def scan(text: String) = {
    val text_to_chars = text.toList
    var line_number = 1
    @annotation.tailrec()
    def scan_token(chars: List[Char], tokens: List[Token]): List[Token]= {
      def makeToken(tokenType: TokenType, char: Char) = {
        Token(tokenType, char.toString, Some(char), line_number) :: tokens
      }
      def makeTokens(tokenType: TokenType, chars: String) = {
        Token(tokenType, chars, Some(chars), line_number) :: tokens
      }
      chars match {
        // EOF
        case Nil => makeToken(TokenType.EOF, 'x')
        // Whitespace
        case (' ' | '\r' | '\t')::rest => scan_token(rest, tokens)
        case '\n'::rest=> {
          line_number += 1
          scan_token(rest, tokens)
        }
        // Single Characters
        case (c @ '(' ):: rest => scan_token(rest, makeToken(TokenType.LEFT_PAREN, c))
        case (c @ ')' ):: rest => scan_token(rest, makeToken(TokenType.RIGHT_PAREN, c))
        case (c @ '[' ):: rest => scan_token(rest, makeToken(TokenType.LEFT_PAREN, c))
        case (c @ ']' ):: rest => scan_token(rest, makeToken(TokenType.RIGHT_PAREN, c))
        case (c @ ',' ):: rest => scan_token(rest, makeToken(TokenType.COMMA, c))
        case (c @ '.' ):: rest => scan_token(rest, makeToken(TokenType.DOT, c))
        case (c @ '-' ):: rest => scan_token(rest, makeToken(TokenType.MINUS, c))
        case (c @ '+' ):: rest => scan_token(rest, makeToken(TokenType.PLUS, c))
        case (c @ ';' ):: rest => scan_token(rest, makeToken(TokenType.SEMICOLON, c))
        case (c @ '/' ):: rest => scan_token(rest, makeToken(TokenType.SEMICOLON, c))
        case (c @ '*' ):: rest => scan_token(rest, makeToken(TokenType.SEMICOLON, c))
        // Multi Characters
        case '!':: '=' :: rest => scan_token(rest, makeTokens(TokenType.BANG_EQUAL, "!="))
        case '=' :: '=' :: rest => scan_token(rest, makeTokens(TokenType.EQUAL_EQUAL, "=="))
        case '>' :: '=' :: rest => scan_token(rest, makeTokens(TokenType.GREATER_EQUAL, ">="))
        case '<' :: '=' :: rest => scan_token(rest, makeTokens(TokenType.LESS_EQUAL, "<="))
        // Single Characters Ambiguous
        case (c @ '!' ):: rest => scan_token(rest, makeToken(TokenType.BANG, c))
        case (c @ '=' ):: rest => scan_token(rest, makeToken(TokenType.EQUAL, c))
        case (c @ '>' ):: rest => scan_token(rest, makeToken(TokenType.GREATER, c))
        case (c @ '<' ):: rest => scan_token(rest, makeToken(TokenType.LESS, c))
        // Special
        case (c @ '"' ):: rest => {
          var (rest2, literal) = peek_quote(rest, Nil)
          scan_token(rest2, makeTokens(TokenType.STRING, literal.toString))
        }
        case c:: rest if c.isLetter => {
          var (rest2, literal) = peek_alphabetical(rest, Nil)
          val literalAsString = literal.toString
          val keywords = Keywords.keywordTokenTypeMap.get(literalAsString)
          keywords match {
            case Some(keyword) => scan_token(rest2, makeTokens(keyword, literalAsString))
            case _ => scan_token(rest2, makeTokens(TokenType.IDENTIFIER, literalAsString))
          }
        }
        case c:: rest if c.isDigit => {
          var (rest2, literal) = peek_numeric(rest, Nil)
          val literalAsString = literal.toString
          val keywords = Keywords.keywordTokenTypeMap.get(literalAsString)
          keywords match {
            case Some(keyword) => scan_token(rest2, makeTokens(keyword, literalAsString))
            case _ => scan_token(rest2, makeTokens(TokenType.IDENTIFIER, literalAsString))
          }
        }
        // Uknown
        case (c @ _ ):: rest => scan_token(rest, makeToken(TokenType.UNKNOWN, c))      
      }
    }

    scan_token(text_to_chars, Nil).reverse
}


val tokens = scan("while \"billy bob\" var frank")
println(tokens)
