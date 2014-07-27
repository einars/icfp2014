
type code = [
  | `Mov of string * string
    | `Add of string * string
    | `Sub of string * string
    | `Mul of string * string
    | `Div of string * string
    | `And of string * string
    | `Or  of string * string
    | `Xor of string * string
    | `Inc of string
    | `Dec of string

    | `Jlt of string * string * string
    | `Jgt of string * string * string
    | `Jeq of string * string * string
    | `BlockJeq of string * string * code list
    | `BlockJneq of string * string * code list
    | `BlockJgt of string * string * code list
    | `BlockJlt of string * string * code list
    | `WhileEq of string * string * code list
    | `WhileNeq of string * string * code list
    | `WhileGt of string * string * code list
    | `WhileLt of string * string * code list


    | `Call of string
    | `Ret

    | `Jmp of string
    | `Int of string
    | `Hlt

    | `Label of string
    | `Defconst of string * string

  ]




