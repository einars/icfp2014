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

let all_labels = ref (stock_labels() )
let new_labels = ref label_map_empty

let ret_blocks_used = ref 0
let ret_table = ref label_map_empty


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

    | `BlockJneq _ :: rest -> failwith "Unflattened code: blockjneq"
    | `BlockJeq _ :: rest -> failwith "Unflattened code: blockjeq"
    | `BlockJgt _ :: rest -> failwith "Unflattened code: blockjgt"
    | `BlockJlt _ :: rest -> failwith "Unflattened code: blockjlt"
    | `Call _ :: rest -> failwith "Unflattened code: call"
    | `Ret :: rest -> failwith "Unflattened code: ret"

    | h::rest -> _rec (succ pc) rest
  in
  ignore(_rec 0 codes)

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
    | `Jmp _ :: rest -> _rec (pc + 1) rest
    | `Jlt _ :: rest -> _rec (pc + 1) rest
    | `Jgt _ :: rest -> _rec (pc + 1) rest
    | `Jeq _ :: rest -> _rec (pc + 1) rest
    | `Int _ :: rest -> _rec (pc + 1) rest
    | `Hlt :: rest -> _rec (pc + 1) rest
    | `Label _ :: rest -> _rec pc rest
    | `Defconst _ :: rest -> _rec pc rest
    | `BlockJneq _ :: rest -> failwith "Unflattened code: blockjneq"
    | `BlockJeq _ :: rest -> failwith "Unflattened code: blockjeq"
    | `BlockJgt _ :: rest -> failwith "Unflattened code: blockjgt"
    | `BlockJlt _ :: rest -> failwith "Unflattened code: blockjlt"
    | `Call _ :: rest -> failwith "Unflattened code: call"
    | `Ret :: rest -> failwith "Unflattened code: ret"

  in
  _rec 0 c
;;


let random_label_cnt = ref 0

let random_label () =
  random_label_cnt := !random_label_cnt + 1;
  sprintf "lbl$%d" !random_label_cnt

;;


let rec dump_raw code =
  let dump_single = function
  | `Mov (a, b) -> sprintf "mov %s %s" a b
  | `Add (a, b) -> sprintf "add %s %s" a b
  | `Sub (a, b) -> sprintf "sub %s %s" a b
  | `Mul (a, b) -> sprintf "mul %s %s" a b
  | `Div (a, b) -> sprintf "div %s %s" a b
  | `And (a, b) -> sprintf "and %s %s" a b
  | `Or (a, b) -> sprintf "or %s %s" a b
  | `Xor (a, b) -> sprintf "xor %s %s" a b
  | `Defconst (a, b) -> sprintf "defconst %s %s" a b
  | `Inc (a) -> sprintf "inc %s" a
  | `Dec (a) -> sprintf "dec %s" a
  | `Jmp (a) -> sprintf "jmp %s" a
  | `Int (a) -> sprintf "int %s" a
  | `Label (a) -> sprintf "label %s:" a
  | `Call (a) -> sprintf "call %s" a
  | `Hlt -> sprintf "hlt"
  | `Ret -> sprintf "ret"

  | `Jlt (a, b, c) -> sprintf "jlt %s %s %s" a b c
  | `Jgt (a, b, c) -> sprintf "jgt %s %s %s" a b c
  | `Jeq (a, b, c) -> sprintf "jeq %s %s %s" a b c
  | `BlockJeq _ -> sprintf "blockjeq???"
  | `BlockJneq _ -> sprintf "blockjneq???"
  | `BlockJgt _ -> sprintf "blockjgt???"
  | `BlockJlt _ -> sprintf "blockjlt???"
in
  List.iter code (fun elem -> printf "%s\n" (dump_single elem))



let rec flatten code =

  let rec _rec code_so_far = function
    | [] -> code_so_far
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
        _rec ( code_so_far @ [ `Jmp "lbl$rethandler" ] ) rest
    | `Call (addr) :: rest ->

        let label = random_label () in
        ret_blocks_used := succ !ret_blocks_used;
        ret_table := Map.add !ret_table ~key:(string_of_int !ret_blocks_used) ~data:label;
        _rec ( code_so_far @ [`Dec "H"; `Mov ("*H", (string_of_int !ret_blocks_used)); `Jmp addr; `Label label ]) rest

    | foo :: rest -> _rec (code_so_far @ [foo]) rest
  in
  _rec [] code


  

let make_rethandler () =

  let get_retaddress n = match Map.find !ret_table (string_of_int n) with
  | Some s -> s
  | None -> failwith ("Should not happen, unknown address " ^ (string_of_int n))
  in

  if !ret_blocks_used = 0 then []
  else begin
    let l = ref [ `Label "lbl$rethandler"; `Mov ("g", "*h"); `Inc "h" ] in
    for i = 1 to !ret_blocks_used do
      l := !l @ [ `Jeq (get_retaddress i,  "g", string_of_int i) ];
    done;
    !l
  end

let preprocess code =
  [`Mov ("h", "255") ] @ code @ (make_rethandler ())




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
    | `Jmp (t) -> printf "JEQ %s, 0, 0\n" (get_label t); pc := !pc + 1

    | `BlockJneq _ -> failwith "Unflattened code: blockjneq"
    | `BlockJeq _ -> failwith "Unflattened code: blockjeq"
    | `BlockJgt _ -> failwith "Unflattened code: blockjgt"
    | `BlockJlt _ -> failwith "Unflattened code: blockjlt"
    | `Call s -> failwith ("Unprocessed call: call" ^ s)
    | `Ret -> failwith "Unprocessed ret"

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
  let parsed = preprocess (flatten parsed) in
  (* dump_raw parsed *)
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
  Command.basic ~summary:"Parse and display JSON"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop 
  |> Command.run
