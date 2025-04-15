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
    let idat = filter (fun ch -> ch.chunk_type = [73; 68; 65; 84]) !chunks
        |> concat_map (fun ch -> ch.content) in
    Printf.printf "lengt of data bytes = %d\n" (length idat);
    let data = Zlib.decompress idat in
    Printf.printf "length of decompressed data = %d\n" (length data);
    Printf.printf "expected length of decompressed data = %d\n" (height * (1 + width * 4));
    Image.({data = [||]; width = 0; height = 0});;
