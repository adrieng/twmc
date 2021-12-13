{
  open Parser
}

let whitespace = [' ' '\t' '\n']

let id = ['a'-'z' 'A'-'Z']+

rule token = parse
  | whitespace+ { token lexbuf }
  | "top" { TOP }
  | "bot" { BOT }
  | id { ID (Lexing.lexeme lexbuf) }
  | "*" { STAR }
  | "<=" { LE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | eof { EOF }
