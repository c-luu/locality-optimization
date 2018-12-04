(* Types *)
type binary_function = BinaryFunction of string;;
type expression = 
    | Symbol of string
    | Operation of binary_function * expression * expression;;
type line = Line of expression * expression;;
type basic_block = line list;;
type ('k, 'v) lvn = ('k, 'v) Hashtbl.t;;

(* Logging *)
let fail_with msg = raise (Failure msg);;
let lvn_msg_for blk = 
    Printf.printf "LVN Table for Block: %s\n" (string_of_int blk);;
let svn_msg_for ebb = 
    Printf.printf "LVN Table for EBB: %s\n" ebb;;
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

(* Test *)
let add = BinaryFunction "+";;
let sub = BinaryFunction "-";;
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

(*  Local value numbering for basic-blocks. *)
let to_op_key e tbl = 
    match e with
    | Operation (BinaryFunction b, Symbol x, Symbol y) ->
        (b ^ string_of_int (Hashtbl.find tbl x) ^ string_of_int (Hashtbl.find tbl y))  
    | _ ->
        fail_with "Need binary operation for this key form."  

let hash_blk blk seed tbl vn_tbl = 
    let symbol_exists s = 
        Hashtbl.mem tbl s in
    let hash_op o =
        match o with
        | t , b , x , y ->
            if not (symbol_exists (to_op_key (b, x, y))) then
                begin
                    Hashtbl.add tbl (to_op_key (b, x, y)) !seed; 
                    Hashtbl.add tbl t !seed; 
                    Queue.add (b ^ x ^ y, !seed) !vn_tbl;
                    Queue.add (t, !seed) !vn_tbl;
                    incr seed
                end
            else
                begin
                    begin
                        let k = to_op_key (b, x, y) in
                            Hashtbl.add tbl t (Hashtbl.find tbl k);
                            Queue.add (t, (Hashtbl.find tbl k)) !vn_tbl
                    end
                end
    in
    let hash_symb s =
        if not (symbol_exists s) then
            begin
                Hashtbl.add tbl s !seed; 
                Queue.add (s, !seed) !vn_tbl;
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
                    Queue.add (right, !seed) !vn_tbl;
                    Queue.add (left, !seed) !vn_tbl;
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

let test_svn b1 b2 =
    let seed = ref 0 in
    let vn_tbl = ref (Queue.create ()) in
    let lvn_1 = hash_blk b1 seed (Hashtbl.create 123456) vn_tbl in
    let lvn_2 = hash_blk b2 seed (fst lvn_1) (snd lvn_1) in
    Queue.iter (fun x -> Printf.printf "%s -> %d\n" (fst x) (snd x)) !(snd (lvn_2))
;;    

svn_msg_for "[B1, B2]";;
test_svn blk_1 blk_2;;
svn_msg_for "[B1, B3]";;
test_svn blk_1 blk_3;;
