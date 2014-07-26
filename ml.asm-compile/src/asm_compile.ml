(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf

open Core.Std
open Asm_lexer
open Asm_parser
open Lexing

type label_map_t = string String.Map.t
let label_map_empty = String.Map.empty

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Asm_parser.prog Asm_lexer.read lexbuf with
  | Asm_lexer.SyntaxError msg ->
    fprintf stderr "BEBEBE! %s\n" msg;
    exit (-1)
  | Asm_parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)


let stock_labels ()=
  let map = ref label_map_empty in
  for i = 0 to 255 do
    map := Map.add !map ~key:(string_of_int i) ~data:(string_of_int i)
  done;
  map := Map.add !map ~key:"A" ~data:"A";
  map := Map.add !map ~key:"a" ~data:"A";
  map := Map.add !map ~key:"B" ~data:"B";
  map := Map.add !map ~key:"b" ~data:"B";
  map := Map.add !map ~key:"C" ~data:"C";
  map := Map.add !map ~key:"c" ~data:"C";
  map := Map.add !map ~key:"D" ~data:"D";
  map := Map.add !map ~key:"d" ~data:"D";
  map := Map.add !map ~key:"E" ~data:"E";
  map := Map.add !map ~key:"e" ~data:"E";
  map := Map.add !map ~key:"F" ~data:"F";
  map := Map.add !map ~key:"f" ~data:"F";
  map := Map.add !map ~key:"G" ~data:"G";
  map := Map.add !map ~key:"g" ~data:"G";
  map := Map.add !map ~key:"H" ~data:"H";
  map := Map.add !map ~key:"h" ~data:"H";

  !map


let gather_labels codes =
  let map = ref (stock_labels ()) in

  let rec _rec pc = function
    | [] -> !map

    | `Defconst (a,b) :: rest ->
        map := Map.add !map ~key:a ~data:b;
        _rec pc rest;

    | `Label l :: rest ->
        map := Map.add !map ~key:l ~data:(string_of_int pc);
        _rec pc rest;

    | h::rest -> _rec (succ pc) rest
  in
  _rec 0 codes

let print_codes code labels =
  let get_label label =
    match Map.find labels label with
    | None -> failwith ("Unknown label " ^ label)
    | Some s -> s

  in

  let iter_fn = function
    | `Mov (a, b) -> printf "MOV %s, %s\n" (get_label a) (get_label b)
    | `Add (a, b) -> printf "ADD %s, %s\n" (get_label a) (get_label b)
    | `Sub (a, b) -> printf "SUB %s, %s\n" (get_label a) (get_label b)
    | `Mul (a, b) -> printf "MUL %s, %s\n" (get_label a) (get_label b)
    | `Div (a, b) -> printf "DIV %s, %s\n" (get_label a) (get_label b)
    | `And (a, b) -> printf "AND %s, %s\n" (get_label a) (get_label b)
    | `Or (a, b) -> printf "OR %s, %s\n" (get_label a) (get_label b)
    | `Xor (a, b) -> printf "XOR %s, %s\n" (get_label a) (get_label b)
    | `Inc (a) -> printf "INC %s\n" (get_label a)
    | `Dec (a) -> printf "DEC %s\n" (get_label a)

    | `Jlt (a, b, c) -> printf "JLT %s, %s, %s\n" (get_label a) (get_label b) (get_label c)
    | `Jgt (a, b, c) -> printf "JGT %s, %s, %s\n" (get_label a) (get_label b) (get_label c)
    | `Jeq (a, b, c) -> printf "JEQ %s, %s, %s\n" (get_label a) (get_label b) (get_label c)

    | `Int i -> printf "INT %s\n" (get_label i)
    | `Hlt -> printf "HLT\n";
    | `Label l -> ()
    | `Defconst l -> ()
  in

  List.iter code iter_fn 


let rec parse_and_print lexbuf =
  let parsed = parse_with_error lexbuf in
  let labels = gather_labels parsed in
  print_codes parsed labels


let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx


let () =
  Command.basic ~summary:"Parse and display JSON"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop 
  |> Command.run
