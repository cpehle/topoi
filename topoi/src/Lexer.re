
open Token;

open Lexbuf;

let fail
    {Position.pos_start: pos_start, Position.pos_end: pos_end}
    s
    :Result.t Token.t (Parse_error.t, Position.t, Position.t) =>
  Result.Error (Parse_error.LexError s) pos_start pos_end [@implicit_arity];

let rec token
        ({stream, pos_end} as lexbuf)
        :Result.t Token.t (Parse_error.t, Position.t, Position.t) => {
  let f () => update_position lexbuf;
  let hexdig = [%sedlex.regexp ? '0'..'9' | 'a'..'f' | 'A'..'F'];
  let bin = [%sedlex.regexp ? ("0b", Plus ('0' | '1' | '_'))];
  let hex = [%sedlex.regexp ? ("0x", Plus ('0'..'9' | 'a'..'f' | 'A'..'F' | '_'))];
  let decimal = [%sedlex.regexp ? ('0'..'9', Star ('0'..'9' | '_'))];
  let flo = [%sedlex.regexp
    ?
      (
        '0'..'9',
        Star ('0'..'9' | '_'),
        '.',
        Star ('0'..'9' | '_'),
        Opt ('e' | 'E'),
        Opt ('-' | '+'),
        '0'..'9',
        Star ('0'..'9' | '_')
      ) |
      (
        '0'..'9',
        Star ('0'..'9' | '_'),
        'e' | 'E',
        Opt ('-' | '+'),
        '0'..'9',
        Star ('0'..'9' | '_')
      )
  ];
  let ident = [%sedlex.regexp ? (id_start, Star id_continue)];
  let atom = [%sedlex.regexp ? ident | Plus sm];
  let integer = [%sedlex.regexp ? Plus '0'..'9'];
  [%sedlex
    switch stream {
    | Plus white_space =>
      f ();
      token lexbuf
    | '\n' =>
      f ();
      new_line lexbuf;
      token lexbuf
    | ("--", Compl '\n') =>
      f ();
      comment lexbuf
    | "fn" =>
      f ();
      Result.Ok FUN
    | "let" =>
      f ();
      Result.Ok LET
    | "in" =>
      f ();
      Result.Ok IN
    | "forall" =>
      f ();
      Result.Ok FORALL
    | "module" =>
      f ();
      Result.Ok MODULE
    | flo =>
      f ();
      Result.Ok (FLOAT (Float.of_string (lexeme lexbuf)))
    | bin
    | hex
    | decimal =>
      f ();
      Result.Ok (INT (Int64.of_string (lexeme lexbuf)))
    | ('"', Plus (Compl '"'), '"') =>
      f ();
      let s = lexeme lexbuf;
      Result.Ok (STRING (String.sub s 1 (String.length s - 2)))
    | '(' =>
      f ();
      Result.Ok LPAREN
    | ')' =>
      f ();
      Result.Ok RPAREN
    | '[' =>
      f ();
      Result.Ok LBRACKET
    | ']' =>
      f ();
      Result.Ok RBRACKET
    | '{' =>
      f ();
      Result.Ok LBRACE
    | '}' =>
      f ();
      Result.Ok RBRACE
    | '=' =>
      f ();
      Result.Ok EQUALS
    | "->"
    | 8594
    | 10230 =>
      f ();
      Result.Ok RIGHTARROW
    | '.' =>
      f ();
      Result.Ok DOT
    | ident =>
      f ();
      Result.Ok (IDENT (lexeme lexbuf))
    | atom =>
      f ();
      Result.Ok (ATOM (lexeme lexbuf))
    | eof =>
      f ();
      Result.Ok EOF
    | _ =>
      f ();
      fail lexbuf "Unexpected character"
    }
  ]
}
and comment ({stream, pos_end} as lexbuf) => [%sedlex
  switch stream {
  | '\n' =>
    update_position lexbuf;
    token lexbuf
  | _ =>
    update_position lexbuf;
    comment lexbuf
  }
];
