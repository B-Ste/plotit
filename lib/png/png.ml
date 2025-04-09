module Zlib : sig
    val decompress : int list -> int list
end = struct 
    exception Invalid_compression of string

    let inflate_block_no_compression br out = 
        let open Bitreader in
        let len = read_bytes br 2 in
        ignore @@ read_bytes br 2;
        for _ = 1 to len do
            out := read_byte br :: !out
        done;;

    let inflate_block_fixed _ _ = ()

    let inflate_block_dynamic _ _ = ()

    let decompress l = 
        let open Bitreader in
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
end

open Bitstream
open List

exception Invalid_png

type chunk = {length: int; chunk_type: int list; content: int list; crc: int}

let png_signature = map (fun x -> Bitstream.ConstUInt8 x) [137; 80; 78; 71; 13; 10; 26; 10]

let rec create_parse_list = function
    | 0 -> []
    | n -> Bitstream.V Bitstream.UInt8 :: create_parse_list (n - 1)

let parse_chunk bs = 
    let len = match_single bs (V Int32_BE) in
    let t = match_pattern bs [V UInt8; V UInt8; V UInt8; V UInt8] in
    let con = match_pattern bs (create_parse_list len) in
    let crc = match_single bs (V Int32_BE) in
    {length = len; chunk_type = t; content = con; crc = crc};;

let combine = fold_left (fun a x -> 256 * a + x) 0

let decode_png s = 
    let bs = open_bitstream s in
        (* Check presence of signature and extract all chunks. *)
        if not @@ match_const_pattern bs png_signature then raise Invalid_png;
        let chunks = ref [] in
        while (not @@ is_finished bs) do
            chunks := parse_chunk bs :: !chunks;
        done;
        chunks := rev !chunks;

        (* Check first chunk to be the header and extract contained data. *)
        let head = hd !chunks in
        if not @@ (head.chunk_type = [73; 72; 68; 82]) then raise Invalid_png;
        let width = combine @@ take 4 head.content in
        let height = combine @@ take 4 (drop 4 head.content) in
        let bit_depth = hd (drop 8 head.content) in
        let color_type = hd (drop 9 head.content) in
        let compression_method = hd (drop 10 head.content) in
        let filter_method = hd (drop 11 head.content) in
        let interlace_metod = hd (drop 12 head.content) in
        Printf.printf "width = %d, height = %d\n" width height;
        Printf.printf "bit_depth = %d\n" bit_depth;
        Printf.printf "color_type = %d\n" color_type;
        Printf.printf "compression_metho = %d\n" compression_method;
        Printf.printf "filter_method = %d\n" filter_method;
        Printf.printf "interlace_method = %d\n" interlace_metod;
        Image.({data = [||]; width = 0; height = 0});;
