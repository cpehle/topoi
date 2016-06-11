
type reg = | RAX | RCX | RDX | RBX | RSP | RSB | RSI | RDI;

type value = | I of Int64.t | R of reg | A of Int64.t;

type instr =
  | Ret
  | Mov of value value
  | Add of value value
  | Sub of value value
  | Mul of value value
  | IMul of value value
  | Xor of value value
  | Inc of value
  | Dec of value
  | Push of value
  | Pop of value
  | Call of value
  | Loop of value
  | Nop
  | Syscall;

type jitmem = {
  instructions: list instr,
  mach: array int,
  icount: int,
  memptr: int,
  memoff: Int64.t
};

let emit: array int => jitmem => jitmem =
  fun i {instructions, mach, icount, memptr, memoff} => {
    instructions,
    mach: Array.append mach i,
    icount,
    memptr,
    memoff: Int64.add memoff (Int64.of_int (Array.length i))
  };

let ret: jitmem => jitmem = fun j => emit [|195|] j;

/* Registers */
let rax: value = R RAX;

let rbp: value = R RBX;

let rsp: value = R RSP;

let rcx: value = R RCX;

let rdx: value = R RDX;

let label: jitmem => value = fun {instructions, mach, icount, memptr, memoff} => A memoff;

let push: value => jitmem => jitmem =
  fun v {instructions, mach, icount, memptr, memoff} =>
    switch v {
    | R r => {instructions, mach, icount, memptr, memoff}
    | _ => {instructions, mach, icount, memptr, memoff}
    };

let pop: value => jitmem => jitmem =
  fun v {instructions, mach, icount, memptr, memoff} =>
    switch v {
    | R r => {instructions, mach, icount, memptr, memoff}
    | _ => {instructions, mach, icount, memptr, memoff}
    };

let mov: value => value => jitmem => jitmem =
  fun v v' s =>
    switch (v, v') {
    | (R r, R r') => emit [||] s
    | _ => emit [||] s
    };

let id i => i;

let prologue: jitmem => jitmem = fun s => id s |> push rbp |> mov rbp rsp;

let epilogue: jitmem => jitmem = fun s => id s |> pop rax |> mov rsp rbp;
