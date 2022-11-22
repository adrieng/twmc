{
  open Parser
}

let whitespace = [' ' '\t' '\n']

let id = ['a'-'z' 'A'-'Z']+

rule token = parse
  | whitespace+ { token lexbuf }
  | "id" { ID }
  | id { IDENT (Lexing.lexeme lexbuf) }
  | "*" { STAR }
  | "/\\" { MEET }
  | "\\/" { JOIN }
  | "/" { SLASH }
  | "\\" { ASLASH }
  | "'" { PRIME }
  | "<=" { LE }
  | "=" { EQ }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | eof { EOF }
