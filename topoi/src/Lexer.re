
let rec token ({stream, pos_end} as lexbuf) => {
  let hexdigit = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A'..'F'];
  let bin = [%sedlex.regexp? "0b", Plus ('0' | '1' | '_')];
  let hex = [%sedlex.regexp? "0x", Plus ('0'..'9' | 'a'..'f' | 'A'..'F' | '_')];
  let decimal = [%sedlex.regexp? '0'..'9', Star ('0'..'9' | '_')];
  let flo = [%sedlex.regexp? ( '0'..'9', Star ('0'..'9' | '_'), '.', Star ('0'..'9' | '_'),  Opt  ('e' | 'E'), Opt ('-' | '+'), '0'..'9', Star ('0'..'9' | '_')) | ( '0'..'9', Star ('0'..'9' | '_'),  ('e' | 'E'), Opt ('-' | '+'), '0'..'9', Star ('0'..'9' | '_'))];
  let ident = [%sedlex.regexp? id_start , Star (id_continue)];


  switch%sedlex stream {
    | "=>" => ()
  };

};
