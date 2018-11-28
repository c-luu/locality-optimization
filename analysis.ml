type binary_function = BinaryFunction of string;;
type expression = 
    | Symbol of string
    | Operation of binary_function * expression * expression;;
type line = Line of expression * expression;;
type basic_block = line list;;
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

(* path_exists : edge -> edge -> cfg -> bool;; *)

(* Local Val Numbering *)
let a = Symbol "a";;
let b = Symbol "b";;
let x = Symbol "x";;
let y = Symbol "y";;
let c = Symbol "c";;
let seventeen = Symbol "17";;
let add = BinaryFunction "+";;
let sub = BinaryFunction "-";;
let line_1 = Line (a, Operation (add, x, y));;
let line_2 = Line (b, Operation (add, x, y));;
let line_3 = Line (a, seventeen);;
let line_4 = Line (c, Operation (add, x, y));;
let blk_0 = [ line_1; line_2; ];;
let blk_1 = [ line_1; line_2; line_3; line_4 ];;

let hash_block blk seed = 
    let seed = ref seed in
    let tbl = Hashtbl.create 123456 in 
    let symbol_exists s = 
        Hashtbl.mem tbl s in
    let hash_line l =
        match l with
        | t , b , x , y ->
            if not (symbol_exists (b ^ x ^ y)) then
                begin
                    Hashtbl.add tbl (b ^ x ^ y) !seed; 
                    Hashtbl.add tbl t !seed; 
                    incr seed
                end
            else
                Hashtbl.add tbl t (Hashtbl.find tbl (b ^ x ^ y)) 
    in
    let hash_symb s =
        if not (symbol_exists s) then
            begin
                Hashtbl.add tbl s !seed; 
                incr seed
            end
    in
    let hash l = 
        match l with
        | Line (Symbol t, Operation (BinaryFunction b, Symbol x, Symbol y)) ->
            begin
                hash_symb x;
                hash_symb y;
                hash_line (t, b, x, y)
            end
        | Line (Symbol x, Symbol y) ->
            begin
                hash_symb y;
            end
    in
    List.iter hash blk;
    tbl;;

let test_harness blk = 
    hash_block blk 1;;

Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x y) (test_harness blk_1);;
