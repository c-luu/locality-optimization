type op = 
    | Add
    | Sub
;;

type symbol =
    | Constant of int
    | Name of string
;;

type rhs = 
    | Symbol of symbol
    | Expr of op * symbol * symbol
    | RHS of rhs
;;    

let lvn = Hashtbl.create 123456;;

let insert = function
    | RHS c -> Hashtbl.replace lvn c 0
;;

