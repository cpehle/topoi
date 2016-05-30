
type id = {name: string};

type iid = int;

type unop = | Negate;

type ssa_type = | I of int | F of int;

type binop = | Add | Multiply | Reshape;

type literal = | Float of float | Int of int;

type expression = | Literal of literal | Id of id | Unop of unop iid | Binop of binop iid iid;

type instruction = | Load of string string | Store of string string | Move of string string;

type basic_block = {label: string, input: list (id, ssa_type), instructions: list instruction};

let pp_ssa_type t =>
  switch t {
  | F i => String.concat "" ["r", string_of_int i]
  | I i => String.concat "" ["i", string_of_int i]
  };

let pp_literal l =>
  switch l {
  | Float f => string_of_float f
  | Int i => string_of_int i
  };

let pp_expression e =>
  switch e {
  | Literal l => pp_literal l
  | Id i => i.name
  | Unop up i => ""
  | Binop op i i' => ""
  };

let pp_instruction i =>
  switch i {
  | Load t v => String.concat " " ["load", t, v]
  | Store t v => String.concat " " ["store", t, v]
  | Move t v => String.concat " " ["move", t, v]
  };

let pp_input_list l =>
  String.concat " " (List.map (fun (i, t) => String.concat ":" [i.name, pp_ssa_type t]) l);

let pp_basic_block {label, input, instructions} =>
  String.concat
    ""
    [
      label,
      "(",
      pp_input_list input,
      ") {\n",
      String.concat ";\n" (List.map pp_instruction instructions),
      "}"
    ];
