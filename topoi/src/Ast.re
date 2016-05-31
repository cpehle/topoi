
type id = int;

type cat = | Arrow of cat cat | Id of id | Prod of cat cat;

type literal = | Float of float | Int of int | String of string;

type variable = {name: string};

type term =
  | Var of variable
  | Literal of literal
  | Prod of (list term)
  | Comp of (list term)
  | Fun of variable variable term;

type environment = {vars: list (variable, term)};

let rec eval env t =>
  switch t {
  | Var v => List.assoc v env.vars
  | Literal l => Literal l
  | Prod l => Prod (List.map (eval env) l)
  };

let rec pp_term t =>
  switch t {
  | Var v => v.name
  | Literal l =>
    switch l {
    | Float f => string_of_float f
    | Int i => string_of_int i
    | String s => s
    }
  | Prod l => String.concat " " (List.map pp_term l)
  | Comp l => String.concat " " (List.map pp_term l)
  | Fun v v' tm => String.concat " " ["fn", v.name, "(", v'.name, ")", "=>", pp_term tm]
  };

let module Test = {
  open OUnit;
  let t1 =
    Comp [Prod [Var {name: "f"}, Var {name: "id"}], Prod [Var {name: "id"}, Var {name: "g"}]];
};
