type op = 
    | Add
    | Sub
;;

type symbol =
    | Constant of int
    | Name of string
;;

type expression = 
    | Symbol of symbol
    | Operation of op * symbol * symbol
;;    

type line = string * expression
;;

type basic_blocks = Lines of line list
;;

