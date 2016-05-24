
type literal = | Float of float | Int of int | String of string;

type variable = {name: string};

type term =
  | Var of variable
  | Literal of literal
  | Prod of (list term)
  | Comp of (list term)
  | Fun of variable variable term;

let rec to_string t =>
  switch t {
  | Var v => v.name
  | Literal l =>
    switch l {
    | Float f => string_of_float f
    | Int i => string_of_int i
    | String s => s
    }
  | Prod l => String.concat " " (List.map to_string l)
  | Comp l => String.concat " " (List.map to_string l)
  | Fun v v' tm => String.concat " " ["fn", v.name, "(", v'.name, ")", "=>", to_string tm]
  };
