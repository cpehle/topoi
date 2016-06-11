
type t = {filename: Option.t string, line: int, lineoffset: int, columnoffset: int};

let default_position = {filename: None, line: 1, lineoffset: 0, columnoffset: 0};

let to_string p => {
  let fn =
    switch p.filename {
    | None => "<none>"
    | Some f => "<" ^ f ^ ">"
    };
  sprintf
    "{ filename = %s;line = %i;lineoffset = %i;columnoffset = %i}"
    fn
    p.line
    p.lineoffset
    p.columnoffset
};
