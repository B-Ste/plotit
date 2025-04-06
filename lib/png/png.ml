exception Invalid_png

type chunk = {length: int; chunk_type: int list; content: int list; crc: int}

let png_signature = List.map (fun x -> Bitstream.ConstUInt8 x) [137; 80; 78; 71; 13; 10; 26; 10]

let rec create_parse_list = function
    | 0 -> []
    | n -> Bitstream.V Bitstream.UInt8 :: create_parse_list (n - 1)

let parse_chunk bs = 
    let open Bitstream in
        let len = match_single bs (V Int32_BE) in
        let t = match_pattern bs [V UInt8; V UInt8; V UInt8; V UInt8] in
        let con = match_pattern bs (create_parse_list len) in
        let crc = match_single bs (V Int32_BE) in
        {length = len; chunk_type = t; content = con; crc = crc};;

let decode_png s = 
    let bs = Bitstream.open_bitstream s in
        if not @@ Bitstream.match_const_pattern bs png_signature then raise Invalid_png;
        let chunks = ref [] in
        while (not @@ Bitstream.is_finished bs) do
            chunks := parse_chunk bs :: !chunks;
        done;
        Printf.printf "%i\n" (List.length !chunks);
        Image.({data = [||]; width = 0; height = 0});;
