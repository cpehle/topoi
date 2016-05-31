
open Cmdliner;

let revolt () => {
  print_string (Ast.pp_term (Ast.Literal (Ast.Float 1.0)));
  print_string "\n"
};

let revolt_term = Term.(const revolt $ const ());

switch (Term.eval (revolt_term, Term.info "revolt")) {
| `Error _ => exit 1
| _ => exit 0
};

let module Test = {open OUnit;};
