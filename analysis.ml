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
(*
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
*)
(* Test 2 *)
(*let a = Symbol "a";;
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

(* Test *)
let a = Symbol "a";;
let b = Symbol "b";;
let c = Symbol "c";;
let d = Symbol "d";;
let p = Symbol "p";;
let q = Symbol "q";;
let t1 = Symbol "t1";;
let t2 = Symbol "t2";;
let t3 = Symbol "t3";;
let t4 = Symbol "t4";;
let t5 = Symbol "t5";;
let t6 = Symbol "t6";;
let blk_1 = [ 
                    Line (p, a); 
                                    Line (q, b); 
                                                    Line (t1, Operation (add, a, b)); 
                                                                    Line (t2, Operation (add, a, b)); 
                                                                                ];;
let blk_2 = [ 
                    Line (t3, Operation (add, p, q)); 
                                    Line (c, Operation (sub, a, b)); 
                                                ];;
let blk_3 = [ 
                    Line (t4, Operation (add, a, b)); 
                                    Line (d, Operation (sub, p, q)); 
];;
let blk_4 = [ 
                    Line (t5, Operation (add, p, q)); 
                                    Line (t6, Operation (sub, p, q)); 
];;

let hash_block blk seed = 
    let seed = ref seed in
    let vn_tbl = Queue.create () in
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
    (tbl, vn_tbl);;

let test_lvn blk seed = 
    hash_block blk seed;;

(*Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x y) (fst (test_harness blk_0 0));;*)
lvn_msg_for 1;;
Queue.iter (fun x -> Printf.printf "%s -> %d\n" (fst x) (snd x)) (snd (test_lvn blk_1 0));;
lvn_msg_for 2;;
Queue.iter (fun x -> Printf.printf "%s -> %d\n" (fst x) (snd x)) (snd (test_lvn blk_2 0));;
lvn_msg_for 3;;
Queue.iter (fun x -> Printf.printf "%s -> %d\n" (fst x) (snd x)) (snd (test_lvn blk_3 0));;
lvn_msg_for 4;;
Queue.iter (fun x -> Printf.printf "%s -> %d\n" (fst x) (snd x)) (snd (test_lvn blk_4 0));;
