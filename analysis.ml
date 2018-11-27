type op = 
    | Add
    | Sub;;
type symbol = Symbol of string;;
type expression = 
     Operation of op * symbol * symbol;;
type line = string * expression;;
type basic_block = Lines of line list;;
type ('k, 'v) lvn = ('k, 'v) Hashtbl.t;;

(* CFG *)
type node = 
    | Block of basic_block
    | EntryNode of basic_block
    | ExitNode of basic_block;;
type nodes = node list;
type edge = node * node;;
type edges = edge list;;
type cfg = nodes * edges;;

(* build_lvn : basic_block) ->  lvn;; *)
(* path_exists : edge -> edge -> cfg -> bool;; *)
