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
  fprintf outx "%s line %d" pos.pos_fname pos.pos_lnum

let parse_with_error lexbuf =
  try Asm_parser.prog Asm_lexer.read lexbuf with
  | Asm_lexer.SyntaxError msg ->
    fprintf stderr "%s\n" msg;
    exit (-1)
  | Asm_parser.Error ->
    fprintf stderr "Syntax error at %a\n" print_position lexbuf;
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


let add_to_map map k v =
  match Map.find map k with
  | None -> Map.add map ~key:k ~data:v
  | Some s -> failwith ("Attempting to redefine \"" ^ k ^ "\" which is " ^ s)

let gather_labels codes =
  let map = ref (stock_labels ()) in

  let rec _rec pc = function
    | [] -> !map

    | `Defconst (a,b) :: rest ->
        map := add_to_map !map a b;
        _rec pc rest;

    | `Label l :: rest ->
        map := add_to_map !map l (string_of_int pc);
        _rec pc rest;

    | h::rest -> _rec (succ pc) rest
  in
  _rec 0 codes

let calc_code_size c =
  let rec _rec pc = function
    | [] -> pc
    | `Mov _ :: rest -> _rec (pc + 1) rest
    | `Add _ :: rest -> _rec (pc + 1) rest
    | `Sub _ :: rest -> _rec (pc + 1) rest
    | `Mul _ :: rest -> _rec (pc + 1) rest
    | `Div _ :: rest -> _rec (pc + 1) rest
    | `And _ :: rest -> _rec (pc + 1) rest
    | `Or _ :: rest -> _rec (pc + 1) rest
    | `Xor _ :: rest -> _rec (pc + 1) rest
    | `Inc _ :: rest -> _rec (pc + 1) rest
    | `Dec _ :: rest -> _rec (pc + 1) rest
    | `Jlt _ :: rest -> _rec (pc + 1) rest
    | `Jgt _ :: rest -> _rec (pc + 1) rest
    | `Jeq _ :: rest -> _rec (pc + 1) rest
    | `Int _ :: rest -> _rec (pc + 1) rest
    | `Hlt :: rest -> _rec (pc + 1) rest
    | `Label _ :: rest -> _rec pc rest
    | `Defconst _ :: rest -> _rec pc rest
    | `BlockJeq (a, b, codes) :: rest -> _rec (1 + _rec 0 codes) rest
    | `BlockJgt (a, b, codes) :: rest -> _rec (1 + _rec 0 codes) rest
    | `BlockJlt (a, b, codes) :: rest -> _rec (1 + _rec 0 codes) rest

  in
  _rec 0 c
;;

let print_codes code labels =
  let rec get_label (label:string) =
    if (label.[0] = '*') then
      sprintf "[%s]" (get_label (String.drop_prefix label 1))
    else (match Map.find labels label with
    | None -> failwith ("Unknown label " ^ label)
    | Some s -> s)

  in

  let pc = ref 0 in

  let rec iter_fn = function

    | `Mov (a, b) -> printf "MOV %s, %s\n" (get_label a) (get_label b); pc := !pc + 1
    | `Add (a, b) -> printf "ADD %s, %s\n" (get_label a) (get_label b); pc := !pc + 1
    | `Sub (a, b) -> printf "SUB %s, %s\n" (get_label a) (get_label b); pc := !pc + 1
    | `Mul (a, b) -> printf "MUL %s, %s\n" (get_label a) (get_label b); pc := !pc + 1
    | `Div (a, b) -> printf "DIV %s, %s\n" (get_label a) (get_label b); pc := !pc + 1
    | `And (a, b) -> printf "AND %s, %s\n" (get_label a) (get_label b); pc := !pc + 1
    | `Or (a, b)  -> printf "OR %s, %s\n" (get_label a) (get_label b); pc := !pc + 1
    | `Xor (a, b) -> printf "XOR %s, %s\n" (get_label a) (get_label b); pc := !pc + 1
    | `Inc (a)    -> printf "INC %s\n" (get_label a); pc := !pc + 1
    | `Dec (a)    -> printf "DEC %s\n" (get_label a); pc := !pc + 1

    | `Jlt (a, b, c) -> printf "JLT %s, %s, %s\n" (get_label a) (get_label b) (get_label c); pc := !pc + 1
    | `Jgt (a, b, c) -> printf "JGT %s, %s, %s\n" (get_label a) (get_label b) (get_label c); pc := !pc + 1
    | `Jeq (a, b, c) -> printf "JEQ %s, %s, %s\n" (get_label a) (get_label b) (get_label c); pc := !pc + 1

    | `BlockJeq (a, b, codes) ->
        let code_size = calc_code_size codes in
        printf "JEQ %d, %s, %s\n" (!pc + code_size + 1) (get_label a) (get_label b);
        pc := !pc + + 1;
        List.iter codes iter_fn;

    | `BlockJgt (a, b, codes) ->
        let code_size = calc_code_size codes in
        printf "JGT %d, %s, %s\n" (!pc + code_size + 1) (get_label a) (get_label b);
        pc := !pc + + 1;
        List.iter codes iter_fn;

    | `BlockJlt (a, b, codes) ->
        let code_size = calc_code_size codes in
        printf "JLT %d, %s, %s\n" (!pc + code_size + 1) (get_label a) (get_label b);
        pc := !pc + + 1;
        List.iter codes iter_fn;


    | `Int i -> printf "INT %s\n" (get_label i); pc := !pc + 1
    | `Hlt -> printf "HLT\n"; pc := !pc + 1
    | `Label l -> ()
    | `Defconst l -> ()
  in

  List.iter code iter_fn ;

  printf "; code size = %d\n" (calc_code_size code)
;;


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
