{
  open Parser
}

let whitespace = [' ' '\t' '\n']

let id = ['a'-'z' 'A'-'Z']+

rule token = parse
  | whitespace+ { token lexbuf }
  | "id" { ID }
  | "top" { TOP }
  | "bot" { BOT }
  | "^o" { EO }
  | "^r" { ER }
  | "^l" { EL }
  | id { IDENT (Lexing.lexeme lexbuf) }
  | "*" { STAR }
  | "/\\" { MEET }
  | "\\/" { JOIN }
  | "/" { SLASH }
  | "\\" { ASLASH }
  | "<=" { LE }
  | "=" { EQ }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | eof { EOF }
