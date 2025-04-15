open Bitreader
open Huffman

exception Invalid_compression of string

let lengthExtraBits = [|0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 5; 5; 5; 5; 0|];;
let lengthBase = [|3; 4; 5; 6; 7; 8; 9; 10; 11; 13; 15; 17; 19; 23; 27; 31; 35; 43; 51; 59; 67; 83; 99; 115; 131; 163; 195; 227; 258|];;
let distanceExtraBits = [|0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 13; 13|];;
let distanceBase = [|1; 2; 3; 4; 5; 7; 9; 13; 17; 25; 33; 49; 65; 97; 129; 193; 257; 385; 513; 769; 1025; 1537; 
    2049; 3073; 4097; 6145; 8193; 12289; 16385; 24577|];;
let codeLengthCodesOrder = [|16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15|];;

let max = 
    let rec max_acc acc = 
        function
        | [] -> acc
        | (h :: t) -> if h > acc then max_acc h t else max_acc acc t
    in
    function
    | [] -> 0
    | (h :: t) -> max_acc h t

let count l n = 
    let o = Array.make (n + 1) 0 in
    ignore @@ List.map (fun x -> o.(x) <- o.(x) + 1) l;
    o;;

let rec zip a b =
    match a, b with
    | (a' :: as'), (b' :: bs') -> (a', b') :: zip as' bs'
    | _, _ -> []

let range n = 
    let rec range_helper = 
        function
        | 0 -> []
        | n -> (n - 1) :: range_helper (n - 1)
    in List.rev (range_helper n);;

let rec repeat a =
    function
    | 0 -> []
    | n -> a :: repeat a (n - 1)

(* creates a huffman-tree out of a list of bit-length list bl and a list of chars beeing the corresponding alphabet. *)
let bl_list_to_tree bl alphabet =
    let max_bits = max bl in
    let bl_count = count bl max_bits in
    let next_code = ref [0; 0] in
    for i = 2 to max_bits do
        next_code := ((List.hd !next_code + bl_count.(i - 1)) lsl 1 ) :: !next_code
    done;
    next_code := List.rev !next_code;
    let next_code_a = Array.of_list !next_code in
    let t = ref create_huffman_tree in
    ignore @@ List.map (fun (a, b) -> if b <> 0 then (
            t := insert a !t next_code_a.(b) b;
            next_code_a.(b) <- next_code_a.(b) + 1;
        ) else ()) (zip alphabet bl);
    !t;;

let decode_trees br = 
    let hlit = (read_bits br 5) + 257 in
    let hdist = (read_bits br 5) + 1 in
    let hclen = (read_bits br 4) + 4 in
    let code_length_tree_bl = Array.make 19 0 in
    for i = 0 to hclen - 1 do
        code_length_tree_bl.(codeLengthCodesOrder.(i)) <- read_bits br 3
    done;
    let code_length_tree = bl_list_to_tree (Array.to_list code_length_tree_bl) (range 19) in
    let bl = ref [] in
    while List.length !bl < hlit + hdist do
        let sym = decode code_length_tree br in
        match sym with
        | 16 -> let prev_code_length = List.hd !bl in
            let repeat_length = (read_bits br 2) + 3 in
            for _ = 1 to repeat_length do
                bl := prev_code_length :: !bl;
            done;
        | 17 -> let repeat_length = (read_bits br 3) + 3 in
            for _ = 1 to repeat_length do
                bl := 0 :: !bl
            done;
        | 18 -> let repeat_length = (read_bits br 7) + 11 in
            for _ = 1 to repeat_length do
                bl := 0 :: !bl
            done;
        | s -> if s <= 15 && s >= 0 then bl := s :: !bl else raise @@ invalid_arg "invalid symbol in huffman decoding."
    done;
    bl := List.rev !bl;
    let literal_length_tree = bl_list_to_tree (List.take hlit !bl) (range 286) in
    let distance_tree = bl_list_to_tree (List.drop hlit !bl) (range 30) in
    (literal_length_tree, distance_tree);; 


let rec inflate_block_data br literal_length_tree distance_tree out = 
    let sym = decode literal_length_tree br in
    if sym = 256 then ()
    else if sym <= 256 then
        (out := sym :: !out;
        inflate_block_data br literal_length_tree distance_tree out;)
    else
        (let sym_c = sym - 257 in
        let length = read_bits br (lengthExtraBits.(sym_c)) + lengthBase.(sym_c) in
        let dist_sym = decode distance_tree br in
        let dist = read_bits br (distanceExtraBits.(dist_sym)) + distanceBase.(dist_sym) in
        for _ = 1 to length do

            (* possible bug here due to -1. *)

            out := List.nth !out (dist - 1) :: !out
        done;
        inflate_block_data br literal_length_tree distance_tree out);;

let inflate_block_no_compression br out = 
    let len = read_bytes br 2 in
    ignore @@ read_bytes br 2;
    for _ = 1 to len do
        out := read_byte br :: !out
    done;;

let inflate_block_fixed br out = 
    let bl_l = (repeat 8 144) @ (repeat 9 112) @ (repeat 7 24) @ (repeat 8 8) in
    let literal_length_tree = bl_list_to_tree bl_l (range 286) in
    let bl_d = repeat 5 30 in
    let distance_tree = bl_list_to_tree bl_d (range 30) in
    inflate_block_data br literal_length_tree distance_tree out;;

let inflate_block_dynamic br out = 
    let (literal_length_tree, distance_tree) = decode_trees br in
    inflate_block_data br literal_length_tree distance_tree out;;

(** [decompress l] interprets l as a list of 8 bit values and decompresses it according to the
    zlib-algorithm. Raises [Invalid_compression s] on error with [s] detailing the error. *)
let decompress l = 
    let open Int in
    
    (* Check header for supported features *)
    let br = create_bitreader l in
    let cmf = read_byte br in
    let cm = logand cmf 0b1111 in
    if cm <> 8 
        then raise @@ Invalid_compression "CM != 8"
    else 
    let cinfo = logand (shift_right_logical cmf 4) 0b1111 in
    if cinfo > 7 
        then raise @@ Invalid_compression "Window-Size too large"
    else
    let flg = read_byte br in
    if (cmf * 256 + flg) mod 31 <> 0 
        then raise @@ Invalid_compression "Checksum failed"
    else
    if logand (shift_right_logical flg 5) 1 = 1
        then raise @@ Invalid_compression "present dictionary not supported"
    else
    
    (* Inflate algorithm *)
    let befinal = ref 0 in
    let out = ref [] in
    while (!befinal = 0) do
        befinal := read_bit br;
        match read_bits br 2 with
        | 0 -> inflate_block_no_compression br out
        | 1 -> inflate_block_fixed br out
        | 2 -> inflate_block_dynamic br out
        | _ -> raise @@ Invalid_compression "invalid block type"
    done;
    List.rev !out;;
