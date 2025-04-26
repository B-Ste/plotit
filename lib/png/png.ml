open Bitstream
open List

exception Invalid_png of string

type chunk = {chunk_type: int list; content: int list}

let png_signature = map (fun x -> Bitstream.ConstUInt8 x) [137; 80; 78; 71; 13; 10; 26; 10]

let create_parse_list n = List.init n (fun _ -> Bitstream.V Bitstream.UInt8)

let parse_chunk bs = 
    let len = match_single bs (V Int32_BE) in
    let t = match_pattern bs [V UInt8; V UInt8; V UInt8; V UInt8] in
    let con = match_pattern bs (create_parse_list len) in
    let _ = match_single bs (V Int32_BE) in
    {chunk_type = t; content = con};;

let combine = fold_left (fun a x -> 256 * a + x) 0

let paethpredictor a b c =
    let p = a + b - c in
    let pa = abs (p - a) in
    let pb = abs (p - b) in
    let pc = abs (p - c) in
    if pa <= pb && pa <= pc 
        then a
    else if pb <= pc 
        then b
    else c;;

(** [decode s] interprets the file at location [s] as png and tries to decode it. Returns [Image.image] with 
    the decoded png on success and raises [invalid_png] on failure. *)
let decode_png s = 
    let bs = open_bitstream s in
    (* Check presence of signature and extract all chunks. *)
    if not @@ match_const_pattern bs png_signature then raise @@ Invalid_png "png signature missing";
    let chunks = ref [] in
    while (not @@ is_finished bs) do
        chunks := parse_chunk bs :: !chunks;
    done;
    chunks := rev !chunks;

    (* Check first chunk to be the header and extract contained data. *)
    let head = hd !chunks in
    if not @@ (head.chunk_type = [73; 72; 68; 82]) then raise @@ Invalid_png "IHDR chunk missing";
    let width = combine @@ take 4 head.content in
    let height = combine @@ take 4 (drop 4 head.content) in
    let bit_depth = hd (drop 8 head.content) in
    let color_type = hd (drop 9 head.content) in
    let compression_method = hd (drop 10 head.content) in
    let filter_method = hd (drop 11 head.content) in
    let interlace_metod = hd (drop 12 head.content) in
    if bit_depth <> 8 || not (color_type = 6 || color_type = 2) || compression_method <> 0 || filter_method <> 0 || interlace_metod <> 0 
        then raise @@ Invalid_png (Printf.sprintf "We only support bit-depth 8, rgba, compression method 0, filter \ 
        method 0 and no interlacing. This is bit-depth %i, color-type %i, compression method %i, filter method %i \
        and interlacing %i" bit_depth color_type compression_method filter_method interlace_metod)
    else
    let idat = filter (fun ch -> ch.chunk_type = [73; 68; 65; 84]) !chunks
        |> concat_map (fun ch -> ch.content) in
    let data = Zlib.decompress idat in

    (* reverse filtering *)
    let bpp = if color_type = 2 then 3 else 4 in
    let stride = width * bpp in
    let recon = Dynarray.create () in
    let recon_a r c = if c >= bpp then Dynarray.get recon (r * stride + c - bpp) else 0 in
    let recon_b r c = if r > 0 then Dynarray.get recon ((r - 1) * stride + c) else 0 in
    let recon_c r c = if r > 0 && c >= bpp then Dynarray.get recon ((r - 1) * stride + c - bpp) else 0 in
    let i = ref 0 in
    for r = 0 to height - 1 do
        let filter_type = Dynarray.get data !i in
        incr i;
        for c = 0 to stride - 1 do
            let to_filter = Dynarray.get data !i in
            incr i;
            let res = ref 0 in
            (match filter_type with
            | 0 -> res := to_filter
            | 1 -> res := to_filter + recon_a r c
            | 2 -> res := to_filter + recon_b r c
            | 3 -> res := to_filter + (recon_a r c + recon_b r c) / 2
            | 4 -> res := to_filter + paethpredictor (recon_a r c) (recon_b r c) (recon_c r c)
            | _ -> raise @@ invalid_arg "unknown filter type");
            Dynarray.add_last recon (!res land 0xff)
        done;
    done;

    (* convert filtered pixel values to rgb array array. *)
    let m = Array.init_matrix height width (
        fun x y -> let location = x * stride + y * bpp in 
            Image.{r = Dynarray.get recon location; g = Dynarray.get recon (location + 1); b = Dynarray.get recon (location + 2)}
    ) in
    Image.({data = m; width = width; height = height});;
