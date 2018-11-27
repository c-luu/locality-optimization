type binary_function = BinaryFunction of string;;
(*type symbol = Symbol of string;;*)
type expression = 
    | Symbol of string
    | Operation of binary_function * symbol * symbol;;
(*type line = Line of symbol * expression;;*)
type line = Line of Symbol * expression;;
type basic_block = Lines of line list;;
type ('k, 'v) lvn = ('k, 'v) Hashtbl.t;;

(* CFG *)
type node = 
    | Block of basic_block
    | EntryNode of basic_block
    | ExitNode of basic_block;;
type nodes = node list;;
type edge = node * node;;
type edges = edge list;;
type cfg = nodes * edges;;

(* build_lvn : basic_block) ->  lvn;; *)
(* path_exists : edge -> edge -> cfg -> bool;; *)

(* Local Val Numbering *)
let hash_block (blk : basic_block) : (line, int) lvn = 
    let lvn = Hashtbl.create 123456 in 
        let hash_line line = 
            match line with
            | Line (Symbol s, Operation o) ->
                    Hashtbl.add lvn line 1
            | Line (Symbol s, Symbol s) ->
                    Hashtbl.add lvn line 1


(* Original block*)
let a = Symbol "a";;
let b = Symbol "b";;
let c = Symbol "c";;
let d = Symbol "d";;
let add = BinaryFunction "+";;
let sub = BinaryFunction "-";;
let line_1 = Line (a, Operation (add, b, c));;
let line_2 = Line (b, Operation (sub, a, d));;
let line_3 = Line (c, Operation (add, b, c));;
let line_4 = Line (d, Operation (add, a, d));;
let blk_1 = Lines [ line_1; line_2; line_3; line_4 ];
