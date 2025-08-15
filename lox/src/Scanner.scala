package scanner
import token.{Token,TokenType,Keywords}

@annotation.tailrec
def peek_quote(chars: List[Char], literal: List[Char]): (List[Char], List[Char]) = chars match {
  case Nil => (Nil, literal.reverse)
  case '"'::rest => (rest, literal.reverse)
  case char::rest=> peek_quote(rest, char::literal)
}

@annotation.tailrec
def peek_alphabetical(text: List[Char], literal: List[Char]): (List[Char], List[Char]) = text match {
  case Nil => (Nil, literal.reverse)
  case char::rest if char.isLetter || char.isDigit || char == '_' => peek_alphabetical(rest, char::literal)
  case c@_=> (c, literal.reverse)
}

@annotation.tailrec
// TODO: Fix numbers
def peek_numeric(chars: List[Char], literal: List[Char], hasPeriod: Boolean, isValid: Boolean): (List[Char], List[Char], Boolean) = (chars) match {
  case Nil => (Nil, literal.reverse, isValid)
  case (' ' | '\r' | '\t')::rest => (rest, literal.reverse, isValid)
  case (c @ '.')::rest=> hasPeriod match {
    case true => peek_numeric(rest, c::literal, true, false)
    case false => peek_numeric(rest, c::literal, true, isValid)
  }
  case hd::rest if hd.isDigit || hd == '_' => peek_numeric(rest, hd::literal, hasPeriod, isValid)
  case hd::rest => (rest, (hd::literal).reverse, false)
}

object Scanner {
  def scan(text: String): List[Token] = {
    val text_to_chars = text.toList
    var line_number = 1
    @annotation.tailrec()
    def scan_token(chars: List[Char], tokens: List[Token]): List[Token]= {
      // I would overload, but it doesn't seem possible
      def makeToken(tokenType: TokenType, char: Char) = {
        Token(tokenType, char.toString, Some(char), line_number) :: tokens
      }
      def makeTokens(tokenType: TokenType, chars: String) = {
        Token(tokenType, chars, Some(chars), line_number) :: tokens
      }
      def makeEof() = {
        Token(TokenType.EOF, "", None, line_number) :: tokens
      }
      chars match {
        // EOF
        case Nil => makeEof()
        // Whitespace
        case (' ' | '\r' | '\t')::rest => scan_token(rest, tokens)
        case '\n'::rest=> {
          line_number += 1
          scan_token(rest, tokens)
        }
        // Single Characters
        case (c @ '(' ):: rest => scan_token(rest, makeToken(TokenType.LEFT_PAREN, c))
        case (c @ ')' ):: rest => scan_token(rest, makeToken(TokenType.RIGHT_PAREN, c))
        case (c @ '[' ):: rest => scan_token(rest, makeToken(TokenType.LEFT_BRACE, c))
        case (c @ ']' ):: rest => scan_token(rest, makeToken(TokenType.RIGHT_BRACE, c))
        case (c @ ',' ):: rest => scan_token(rest, makeToken(TokenType.COMMA, c))
        case (c @ '.' ):: rest => scan_token(rest, makeToken(TokenType.DOT, c))
        case (c @ '-' ):: rest => scan_token(rest, makeToken(TokenType.MINUS, c))
        case (c @ '+' ):: rest => scan_token(rest, makeToken(TokenType.PLUS, c))
        case (c @ ';' ):: rest => scan_token(rest, makeToken(TokenType.SEMICOLON, c))
        case (c @ '/' ):: rest => scan_token(rest, makeToken(TokenType.SLASH, c))
        case (c @ '*' ):: rest => scan_token(rest, makeToken(TokenType.STAR, c))
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
        case c @ (hd:: rest) if hd.isLetter => {
          var (rest2, literal) = peek_alphabetical(c, Nil)
          val literalAsString = literal.mkString
          val keywords = Keywords.keywordTokenTypeMap.get(literalAsString)
          keywords match {
            case Some(keyword) => scan_token(rest2, makeTokens(keyword, literalAsString))
            case _ => scan_token(rest2, makeTokens(TokenType.IDENTIFIER, literalAsString))
          }
        }
        case c @ (hd:: rest) if hd.isDigit => {
          var (rest2, literal, isValid) = peek_numeric(c, Nil, false, true)
          val literalAsString = literal.mkString
          isValid match {
            case true => scan_token(rest2, makeTokens(TokenType.NUMBER, literalAsString))
            case false => scan_token(rest2, makeTokens(TokenType.UNKNOWN, literalAsString))
          }
        }
        // Uknown
        case (c @ _ ):: rest => scan_token(rest, makeToken(TokenType.UNKNOWN, c))      
      }
    }

    scan_token(text_to_chars, Nil).reverse
}
}
