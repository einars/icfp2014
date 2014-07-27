(* I am a lexer *)

{
  open Lexing
  open Asm_parser
  open Core.Std

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in 
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

  let ident1 = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
  let ident2 = '[' ['a'-'z' 'A'-'Z' '0'-'9' '_']+ ']'
  let white = [' ' '\t']+
  let newline = '\r' | '\n' | '\r' '\n'
  let comment = ';' [ ^ '\r' '\n' ]+
  let int = [ '0'-'9' ]+

  rule read =
    parse
    | white { read lexbuf }
    | newline { next_line lexbuf; read lexbuf }
    | "," { COMMA }
    | comment { read lexbuf }
    (* | ";" { SEMICOLON } *)
    | ":" { COLON }
    | "call" { CMD_CALL }
    | "ret" { CMD_RET }
    | "mov" { CMD_MOV }
    | "inc" { CMD_INC }
    | "dec" { CMD_DEC }
    | "add" { CMD_ADD }
    | "sub" { CMD_SUB }
    | "mul" { CMD_MUL }
    | "div" { CMD_DIV }
    | "and" { CMD_AND }
    | "or"  { CMD_OR }
    | "xor" { CMD_XOR }
    | "jlt" { CMD_JLT }
    | "jgt" { CMD_JGT }
    | "jeq" { CMD_JEQ }
    | "int" { CMD_INT }
    | "hlt" { CMD_HLT }
    | "equ" { EQU }
    | "if" { IF }
    | "jmp" { CMD_JMP }

    | ">" { GT }
    | "<" { LT }

    | "+=" { PLUSEQ }
    | "-=" { MINUSEQ }
    | "*=" { MULTEQ }
    | "/=" { DIVEQ }

    | "=" { EQUALS }
    | "==" { EQUALS }
    | "!=" { NEQUALS }
    | "<>" { NEQUALS }
    | "{" { OPEN_BRACE }
    | "}" { CLOSE_BRACE }

    | ident1 { IDENTIFIER (Lexing.lexeme lexbuf) }
    | ident2 { IDENTIFIER ("*" ^ (String.drop_prefix (String.drop_suffix (Lexing.lexeme lexbuf) 1) 1)) }
    | _ { raise (SyntaxError ("This is not asm: " ^ Lexing.lexeme lexbuf)) }
    | eof { EOF }

