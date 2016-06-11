
type t =
  | LET
  | RPAREN
  | LPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | TINT
  | TBOOL
  | IF
  | THEN
  | ELSE
  | DOT
  | FORALL
  | MATCH
  | MODULE
  | IN
  | WHERE
  | COMMA
  | INT of Int64.t
  | FLOAT of float
  | IDENT of string
  | ATOM of string
  | STRING of string
  | SPACES
  | RIGHTARROW
  | EQUALS
  | FUN
  | EOF
  | MINUS
  | PLUS
  | PIPE
  | COLON;

let to_string t =>
  switch t {
  | LET => "let"
  | RPAREN => ")"
  | LPAREN => "("
  | LBRACKET => "["
  | RBRACKET => "]"
  | LBRACE => "{"
  | RBRACE => "}"
  | DOT => "."
  | IF => "if"
  | THEN => "then"
  | ELSE => "else"
  | TINT => "int"
  | TBOOL => "bool"
  | STRING s => "\"" ^ s ^ "\""
  | FORALL => "forall"
  | MATCH => "match"
  | MODULE => "module"
  | IN => "in"
  | FLOAT f => string_of_float f
  | INT i => Int64.to_string i
  | COMMA => ","
  | IDENT s => "ident(" ^ s ^ ")"
  | ATOM s => "atom(" ^ s ^ ")"
  | RIGHTARROW => "â\134\146"
  | EQUALS => "="
  | FUN => "fun"
  | EOF => "(end of file)"
  | MINUS => "-"
  | PLUS => "+"
  | WHERE => "where"
  | PIPE => "|"
  | SPACES => "<spaces>"
  | COLON => ","
  };
