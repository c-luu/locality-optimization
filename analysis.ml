type binary_function = BinaryFunction of string;;
type expression = 
    | Symbol of string
    | Operation of binary_function * expression * expression;;
type line = Line of expression * expression;;
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
let blk_0 = Lines [ line_1; line_2; ];;
let blk_1 = Lines [ line_1; line_2; line_3; line_4 ];;

let hash_block blk = 
    let seed = ref 0 in
    let tbl = Hashtbl.create 123456 in 
    let symbol_exists s = 
        Hashtbl.mem tbl s
    in
    let hash_line line = 
        match line with
        | Line (Symbol t, Operation (BinaryFunction b, Symbol x, Symbol y)) ->
            if not (symbol_exists x) then 
                begin
                    Hashtbl.add tbl x !seed; 
                    incr seed
                end
            else if not (symbol_exists y) then
                begin
                    Hashtbl.add tbl y !seed; 
                    incr seed
                end
            else if not (symbol_exists (b ^ x ^ y)) then
                begin
                    Hashtbl.add tbl (b ^ x ^ y) !seed; 
                    Hashtbl.add tbl t !seed; 
                    incr seed
                end
            else
                Hashtbl.add tbl t (Hashtbl.find tbl (b ^ x ^ y))
        | Line (Symbol _, Symbol _) ->
                Hashtbl.add tbl "a" 1
        | Line (_, _) ->
                Hashtbl.add tbl "a" 1 
    in
    tbl;;

let test_harness blk = 
    hash_block blk;;
