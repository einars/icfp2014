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
  map := Map.add !map ~key:"PC" ~data:"PC";
  map := Map.add !map ~key:"pc" ~data:"PC";

  !map

let all_labels = ref (stock_labels() )
let new_labels = ref label_map_empty

let add_to_map map k v =
  match Map.find map k with
  | None -> Map.add map ~key:k ~data:v
  | Some s -> failwith ("Attempting to redefine \"" ^ k ^ "\" which is " ^ s)


let gather_labels codes =

  let rec _rec pc = function
    | [] -> pc

    | `Defconst (a,b) :: rest ->
        all_labels := add_to_map !all_labels a b;
        new_labels := add_to_map !new_labels a b;
        _rec pc rest;

    | `Label l :: rest ->
        all_labels := add_to_map !all_labels l (string_of_int pc);
        new_labels := add_to_map !new_labels l (string_of_int pc);
        _rec pc rest;

    | h::rest -> _rec (succ pc) rest
  in
  ignore(_rec 0 codes)

let calc_code_size c =
  let rec _rec pc = function
    | [] -> pc
    | `Label _ :: rest -> _rec pc rest
    | `Defconst _ :: rest -> _rec pc rest
    | n :: rest -> _rec (pc + 1) rest
  in
  _rec 0 c
;;


let random_label_cnt = ref 0

let random_label () =
  random_label_cnt := !random_label_cnt + 1;
  sprintf "#%d" !random_label_cnt

;;


let rec flatten code =

  let rec _rec code_so_far = function
    | [] -> code_so_far
    | `WhileNeq (a, b, codes) :: rest ->
        let l_skip = random_label () in
        let label2 = random_label () in
        _rec ( code_so_far @ [`Label label2; `Jeq (l_skip, a, b);] @ (flatten codes) @ [`Jmp label2; `Label l_skip ]) rest

    | `WhileEq (a, b, codes) :: rest ->
        let l_enter = random_label () in
        let l_skip = random_label () in
        let l_loop = random_label () in
        _rec ( code_so_far @ [`Label l_loop; `Jeq (l_enter, a, b); `Jmp l_skip; `Label l_enter ] @ (flatten codes) @ [`Jmp l_loop; `Label l_skip ]) rest

    | `WhileGt (a, b, codes) :: rest ->
        let l_enter = random_label () in
        let l_skip = random_label () in
        let l_loop = random_label () in
        _rec ( code_so_far @ [`Label l_loop; `Jgt (l_enter, a, b); `Jmp l_skip; `Label l_enter ] @ (flatten codes) @ [`Jmp l_loop; `Label l_skip ]) rest

    | `WhileLt (a, b, codes) :: rest ->
        let l_enter = random_label () in
        let l_skip = random_label () in
        let l_loop = random_label () in
        _rec ( code_so_far @ [`Label l_loop; `Jlt (l_enter, a, b); `Jmp l_skip; `Label l_enter ] @ (flatten codes) @ [`Jmp l_loop; `Label l_skip ]) rest



    | `BlockJneq (a, b, codes) :: rest ->
        let label1 = random_label () in
        _rec ( code_so_far @ [`Jeq (label1, a, b); ] @ (flatten codes) @ [`Label label1 ]) rest
    | `BlockJeq (a, b, codes) :: rest ->
        let label1 = random_label () in
        let label2 = random_label () in
        _rec ( code_so_far @ [`Jeq (label1, a, b); `Jmp label2; `Label label1 ] @ (flatten codes) @ [`Label label2 ]) rest
    | `BlockJgt (a, b, codes) :: rest ->
        let label1 = random_label () in
        let label2 = random_label () in
        _rec ( code_so_far @ [`Jgt (label1, a, b); `Jmp label2; `Label label1 ] @ (flatten codes) @ [`Label label2 ]) rest
    | `BlockJlt (a, b, codes) :: rest ->
        let label1 = random_label () in
        let label2 = random_label () in
        _rec ( code_so_far @ [`Jlt (label1, a, b); `Jmp label2; `Label label1 ] @ (flatten codes) @ [`Label label2 ]) rest
    | `Ret :: rest ->
        _rec ( code_so_far @ [ `Inc "H"; `Mov ("PC", "*H") ] ) rest
    | `Call (addr) :: rest ->

        let label = random_label () in
        _rec (code_so_far @ [ `Mov ("*H", label); `Dec "H"; `Jmp addr; `Label label]) rest

    | foo :: rest -> _rec (code_so_far @ [foo]) rest
  in
  _rec [] code


  

let preprocess code =
  [`Mov ("h", "255") ] @ code




let print_codes code =
  let maybe_convert_to_ref s =
    if (s.[0] = '*') then
      sprintf "[%s]" (String.drop_prefix s 1)
    else s
  in

  let rec get_label (label:string) =
    if (label.[0] = '*') then
      sprintf "[%s]" (get_label (String.drop_prefix label 1))
    else (match Map.find !all_labels label with
    | None -> failwith ("Unknown label " ^ label)
    | Some s -> maybe_convert_to_ref s)

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
    | `Jmp (t) -> printf "MOV PC, %s; %s\n\n" (get_label t) t; pc := !pc + 1


    | `Call s -> failwith ("Unprocessed call: call" ^ s)
    | `Ret -> failwith "Unprocessed ret"

    | `Int i -> printf "INT %s\n" (get_label i); pc := !pc + 1
    | `Hlt -> printf "HLT\n"; pc := !pc + 1
    | `Label l -> ()
    | `Defconst l -> ()
    | _ -> failwith "Something unhandled"
  in


  List.iter code iter_fn ;

  printf "; code size = %d\n" (calc_code_size code)
;;


let rec parse_and_print lexbuf =
  let parsed = parse_with_error lexbuf in
  let parsed = preprocess (flatten parsed) in
  gather_labels parsed;
  print_codes parsed;
  (* Map.iter !new_labels (fun ~key:k ~data:v -> printf "; %s = %s\n" k v); *)
;;


let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx


let () =
  Command.basic ~summary:"ICFP-2014 ghost assembly compiler"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop 
  |> Command.run
