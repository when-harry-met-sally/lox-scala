//> using file "./token.sc"
import token.{Token,TokenType}

def scan(text: String) = {
    val text_to_chars = text.toList
    var line_number = 0
    def scan_token(chars: List[Char], tokens: List[Token]): List[Token]= {
      def makeToken(tokenType: TokenType, char: Char) = {
        Token(tokenType, char.toString, Some(char), line_number) :: tokens
      }
      chars match {
        case Nil => makeToken(TokenType.EOF, 'x')
        case (c @ ',' ):: rest2 => scan_token(rest2, makeToken(TokenType.COMMA, c))
        case (c @ _ ):: rest2 => scan_token(rest2, makeToken(TokenType.UNKNOWN, c))      
      }
    }

    var tokens = scan_token(text_to_chars, Nil)
}

// let scan (text : string) : Token.t list =
//   let parsed_text = String.to_seq text |> List.of_seq in
//   let _, line_number, tokens = _scan parsed_text 1 [] in
//   let eof_token =
//     { Token.kind = Token.EOF; line_number; column_number = 0; literal = None }
//   in
//   List.rev (eof_token :: tokens)


scan("asadsad")
