{
  open Parser
}

let space = [' ' '\t']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''
let atom = ['a'-'z']

rule token = parse
  | atom { ATOM (Lexing.lexeme lexbuf) }
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "!"  { NOT }
  | "and" { AND }
  | "&&" { AND }
  | "or" { OR }
  | "||" { OR }
  | "N" { NEXT }
  | "X" { NEXT }
  | "U" { UNTIL }
  | "G" { GLOBALLY }
  | "F" { FINALLY }
  | "R" { RELEASE }
  
  | ')' { RPAREN }
  | '(' { LPAREN }
  | space+ { token lexbuf }  (* sauter les espaces *)

  | eof { EOF }
