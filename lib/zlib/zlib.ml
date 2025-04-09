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

    (** [decompress l] interprets l as a list of 8 bit values and decompresses it according to the
        zlib-algorithm. Raises [Invalid_compression s] on error with [s] detailing the error. *)
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
