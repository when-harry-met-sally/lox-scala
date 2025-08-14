//> using file "./token.sc"
import token.{Token,TokenType}

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

        case (c @ _ ):: rest => scan_token(rest, makeToken(TokenType.UNKNOWN, c))      
      }
    }

    scan_token(text_to_chars, Nil)
}

// let scan (text : string) : Token.t list =
//   let parsed_text = String.to_seq text |> List.of_seq in
//   let _, line_number, tokens = _scan parsed_text 1 [] in
//   let eof_token =
//     { Token.kind = Token.EOF; line_number; column_number = 0; literal = None }
//   in
//   List.rev (eof_token :: tokens)


val tokens = scan(",asadsad")
println(tokens)
