
type reg = string;

type temp = int;

type label = int;

type ins =
  | Operator of string (list temp) (list temp) (option (list label))
  | Label of string int
  | Move of string temp temp;
