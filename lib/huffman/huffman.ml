type 'a huffmann_tree = 
    | Node of 'a huffmann_tree * 'a huffmann_tree
    | Leaf of 'a
    | Empty

exception Huffman_failure of string

(** Creates and returns a new huffman-tree. *)
let create_huffman_tree = Empty

(** [insert a hf c n] insert element [a] into huffman-tree [hf] at the location specified by codeword [c], which is 
    of length [n]. The MSB of the codeword specifies the first split from the root, while the LSB specifies the last.
    Raises [Huffman_failure] on error. *)
let rec insert a hf c n =
    match n with
    | 0 -> Leaf a
    | _ -> let b = c land (1 lsl (n - 1)) in
        match hf with
        | Node (l, r) -> if b = 1 
            then Node (l, insert a r c (n - 1)) 
            else Node (insert a l c (n - 1), r)
        | Empty -> if b = 1
            then Node (Empty, insert a Empty c (n - 1))
            else Node (insert a Empty c (n - 1), Empty)
        | _ -> raise @@ Huffman_failure "could not insert into huffman tree. "

(** [decode ht br] returns the next element in the huffman-tree [t] on input [br]. *)
let rec decode ht br =
    match ht with
    | Empty -> raise @@ Huffman_failure "could not find fitting element."
    | Leaf a -> a
    | Node (l, r) -> let b = Bitreader.read_bit br in
        match b with
        | 0 -> decode l br
        | _ -> decode r br
