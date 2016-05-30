
open Cmdliner;

print_string (Ast.to_string (Ast.Literal (Ast.Float 1.0)));

print_string "\n";

print_string "Hello World.\n";

let module Test = {open OUnit;};
