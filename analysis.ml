(* Types *)
type binary_function = BinaryFunction of string;;
type expression = 
    | Symbol of string
    | Operation of binary_function * expression * expression;;
type line = Line of expression * expression;;
type basic_block = line list;;
type ('k, 'v) lvn = ('k, 'v) Hashtbl.t;;
type node = 
    | Block of basic_block
    | EntryNode of basic_block
    | ExitNode of basic_block;;
type nodes = node list;;
type edge = node * node;;
type edges = edge list;;
type cfg = nodes * edges;;

(* Utility Functions *)
let fail_with msg = raise (Failure msg);;
let lvn_msg_for blk = 
    Printf.printf "LVN Table for Block: %s\n" (string_of_int blk);;
let print_line line = 
    match line with
    | Line (Symbol t, Operation (BinaryFunction b, Symbol x, Symbol y)) ->
        Printf.printf "%s\n" (t ^ "=" ^ x ^ b ^ y)
    | Line (Symbol x, Symbol y) ->
        Printf.printf "%s\n" (x ^ "=" ^ y)
    | Line (_, _) ->
        fail_with "Main pattern not matched in print function.";;
let print_og_block blk = 
    Printf.printf "Original Block:\n";
    List.iter print_line blk;;

(* Local Value Numbering *)
let add = BinaryFunction "+";;
let sub = BinaryFunction "-";;
(* Test 1 *)
let a = Symbol "a";;
let b = Symbol "b";;
let x = Symbol "x";;
let y = Symbol "y";;
let c = Symbol "c";;
let seventeen = Symbol "17";;
let line_1 = Line (a, Operation (add, x, y));;
let line_2 = Line (b, Operation (add, x, y));;
let line_3 = Line (a, seventeen);;
let line_4 = Line (c, Operation (add, x, y));;
let blk_0 = [ line_1; line_2; line_3; line_4 ];;

(* Test 2 *)
(*
let a = Symbol "a";;
let b = Symbol "b";;
let c = Symbol "c";;
let d = Symbol "d";;
let blk_0 = [ 
                Line (a, Operation (add, b, c)); 
                Line (b, Operation (sub, a, d)); 
                Line (c, Operation (add, b, c)); 
                Line (d, Operation (sub, a, d)); 
            ];;
*)

let hash_block blk seed = 
    let seed = ref seed in
    let vn_tbl = Queue.create () in
    let rewrites = Queue.create () in
    let val_tbl = Hashtbl.create 123456 in
    let tbl = Hashtbl.create 123456 in 
    let symbol_exists s = 
        Hashtbl.mem tbl s in
    let to_op_key k =
        match k with
        | b, x, y ->
            try
                (b ^ string_of_int (Hashtbl.find tbl x) ^ string_of_int (Hashtbl.find tbl y))  
            with e ->
                fail_with (Printexc.to_string e)
    in
    let hash_op o =
        match o with
        | t , b , x , y ->
            if not (symbol_exists (to_op_key (b, x, y))) then
                begin
                    Hashtbl.add tbl (to_op_key (b, x, y)) !seed; 
                    Hashtbl.add tbl t !seed; 
                    Hashtbl.add val_tbl !seed t; 
                    Queue.add (b ^ x ^ y, !seed) vn_tbl;
                    Queue.add (t, !seed) vn_tbl;
                    incr seed
                end
            else
                begin
                    try
                        begin
                            let k = to_op_key (b, x, y) in
                                Hashtbl.add tbl t (Hashtbl.find tbl k);
                                Queue.add (t, (Hashtbl.find val_tbl (Hashtbl.find tbl k))) rewrites;
                                Queue.add (t, (Hashtbl.find tbl k)) vn_tbl
                        end
                    with e ->
                        fail_with (Printexc.to_string e)
                end
    in
    let hash_symb s =
        if not (symbol_exists s) then
            begin
                Hashtbl.add tbl s !seed; 
                Queue.add (s, !seed) vn_tbl;
                incr seed;
            end
    in
    let hash_ass a =
        match a with
        | left, right ->
            if not (symbol_exists right) then
                begin
                    Hashtbl.add tbl right !seed; 
                    Hashtbl.add tbl left !seed; 
                    Queue.add (right, !seed) vn_tbl;
                    Queue.add (left, !seed) vn_tbl;
                    incr seed
                end
    in
    let hash l = 
        match l with
        | Line (Symbol t, Operation (BinaryFunction b, Symbol x, Symbol y)) ->
            begin
                hash_symb x;
                hash_symb y;
                hash_op (t, b, x, y)
            end
        | Line (Symbol x, Symbol y) ->
            begin
                hash_ass (x, y);
            end
        | Line (_, _) ->
            fail_with "Main pattern not matched in hash function."
                
    in
    List.iter hash blk;
    (tbl, (vn_tbl, rewrites));;

let test_lvn blk seed = 
    hash_block blk seed;;

(*Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x y) (fst (test_harness blk_0 0));;*)
lvn_msg_for 0;;
Queue.iter (fun x -> Printf.printf "%s -> %d\n" (fst x) (snd x)) (fst (snd (test_lvn blk_0 1)));;
Queue.iter (fun x -> Printf.printf "%s = %s\n" (fst x) (snd x)) (snd (snd (test_lvn blk_0 1)));;
print_og_block blk_0;;
