%token <string> IDENTIFIER

%token COMMA
%token COLON
%token CMD_MOV
%token CMD_INC
%token CMD_DEC
%token CMD_ADD
%token CMD_SUB
%token CMD_MUL
%token CMD_DIV
%token CMD_AND
%token CMD_OR
%token CMD_XOR
%token CMD_JLT
%token CMD_JGT
%token CMD_JEQ
%token CMD_INT
%token CMD_HLT
%token CMD_JMP

%token EQU IF EQUALS NEQUALS OPEN_BRACE CLOSE_BRACE GT LT PLUSEQ MINUSEQ MULTEQ DIVEQ

%token EOF

%start <Asm_types.code list> prog
%%

prog:
  | ss = stmts; EOF { ss }

stmt:
    | CMD_MOV ; dest = IDENTIFIER; opt_comma; src = IDENTIFIER; { `Mov (dest, src) }

    | IF; x = IDENTIFIER; NEQUALS; y = IDENTIFIER; smts = braced_block; { `BlockJneq (x, y, smts) }
    | IF; x = IDENTIFIER; EQUALS; y = IDENTIFIER; smts = braced_block; { `BlockJeq (x, y, smts) }
    | IF; x = IDENTIFIER; GT; y = IDENTIFIER; smts = braced_block; { `BlockJgt (x, y, smts) }
    | IF; x = IDENTIFIER; LT; y = IDENTIFIER; smts = braced_block; { `BlockJlt (x, y, smts) }

    | dest = IDENTIFIER; EQUALS; src = IDENTIFIER; { `Mov (dest, src) }

    | dest = IDENTIFIER; PLUSEQ; src = IDENTIFIER; { `Add (dest, src) }
    | dest = IDENTIFIER; MINUSEQ; src = IDENTIFIER; { `Sub (dest, src) }
    | dest = IDENTIFIER; MULTEQ; src = IDENTIFIER; { `Mul (dest, src) }
    | dest = IDENTIFIER; DIVEQ; src = IDENTIFIER; { `Div (dest, src) }
    | CMD_ADD ; dest = IDENTIFIER; opt_comma; src = IDENTIFIER; { `Add (dest, src) }
    | CMD_SUB ; dest = IDENTIFIER; opt_comma; src = IDENTIFIER; { `Sub (dest, src) }
    | CMD_MUL ; dest = IDENTIFIER; opt_comma; src = IDENTIFIER; { `Mul (dest, src) }
    | CMD_DIV ; dest = IDENTIFIER; opt_comma; src = IDENTIFIER; { `Div (dest, src) }
    | CMD_AND ; dest = IDENTIFIER; opt_comma; src = IDENTIFIER; { `And (dest, src) }
    | CMD_OR  ; dest = IDENTIFIER; opt_comma; src = IDENTIFIER; { `Or  (dest, src) }
    | CMD_XOR ; dest = IDENTIFIER; opt_comma; src = IDENTIFIER; { `Xor (dest, src) }
    | CMD_INC ; dest = IDENTIFIER;                   { `Inc (dest) }
    | CMD_DEC ; dest = IDENTIFIER;                   { `Dec (dest) }

    | CMD_JMP ; targ = IDENTIFIER; opt_comma; { `Jmp (targ) }
    | CMD_JLT ; targ = IDENTIFIER; opt_comma; x = IDENTIFIER; opt_comma; y = IDENTIFIER { `Jlt (targ, x, y) }
    | CMD_JGT ; targ = IDENTIFIER; opt_comma; x = IDENTIFIER; opt_comma; y = IDENTIFIER { `Jgt (targ, x, y) }
    | CMD_JEQ ; targ = IDENTIFIER; opt_comma; x = IDENTIFIER; opt_comma; y = IDENTIFIER { `Jeq (targ, x, y) }

    | CMD_INT ; i = IDENTIFIER;                                          { `Int (i) }
    | CMD_HLT ;                                                      { `Hlt }

    | id = IDENTIFIER; COLON                 { `Label id }
    | id1 = IDENTIFIER; EQU; id2 = IDENTIFIER { `Defconst (id1, id2) }


braced_block:
  OPEN_BRACE; s = stmts; CLOSE_BRACE; { s }

stmts: ss = rev_stmts { List.rev ss }

opt_comma:
  | { [] }
  | COMMA { [] }

rev_stmts:
  | { [] } (* empty *)
  | ss = rev_stmts; s = stmt { s :: ss }
  ;
