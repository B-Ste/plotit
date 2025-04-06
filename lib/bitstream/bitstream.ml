(** Type for handeling bitstreams. *)
type bitstream = {data: bytes; mutable pos: int; length: int}

(** Type to represent constant values in bitstream. [LE] and [BE] stand for little endian and big endian,
    respectively. *)
type const_pattern_type = 
    | ConstUInt8 of int
    | ConstInt8 of int
    | ConstUInt16_LE of int
    | ConstInt16_LE of int
    | ConstInt32_LE of int
    | ConstUInt16_BE of int
    | ConstInt16_BE of int
    | ConstInt32_BE of int

(** Type to represent variable values in bitstream. [LE] and [BE] stand for little endian and big endian,
    respectively. *)
type var_pattern_type = 
    | UInt8
    | Int8
    | UInt16_LE
    | Int16_LE
    | Int32_LE
    | UInt16_BE
    | Int16_BE
    | Int32_BE

(** Type to represent any values in bitstream. *)
type pattern_type = 
    | C of const_pattern_type
    | V of var_pattern_type

(** [open_bitstream s] opens a bitstream from the file located at [s] and returns the according datastructure. *)
let open_bitstream s = 
    In_channel.with_open_bin s (fun ch ->
        let len = Int64.to_int @@ In_channel.length ch in
        let buf = Bytes.create len in
        ignore @@ In_channel.really_input ch buf 0 len; 
        {data = buf; pos = 0; length = len});;

exception Bitstream_length_exception;;
exception Invalid_int;;

let get f l bs =
    if bs.pos + l <= bs.length then
        let i = f bs.data bs.pos in 
        bs.pos <- bs.pos + l;
        i
    else raise Bitstream_length_exception;;

let get_uint8 = get Bytes.get_uint8 1

let get_int8 = get Bytes.get_int8 1

let get_uint16_le = get Bytes.get_uint16_le 2

let get_int16_le = get Bytes.get_int16_le 2

let get_int32_le bs = get Bytes.get_int32_le 4 bs
    |> Int32.unsigned_to_int
    |> function | Some i -> i | None -> raise Invalid_int;;

let get_uint16_be = get Bytes.get_uint16_be 2

let get_int16_be = get Bytes.get_int16_be 2

let get_int32_be bs = get Bytes.get_int32_be 4 bs 
    |> Int32.unsigned_to_int
    |> function | Some i -> i | None -> raise Invalid_int;;

let match_single_var bs =
    function
    | UInt8 -> get_uint8 bs
    | Int8 -> get_int8 bs
    | UInt16_LE -> get_uint16_le bs
    | Int16_LE -> get_int16_le bs
    | Int32_LE -> get_int32_le bs
    | UInt16_BE -> get_uint16_be bs
    | Int16_BE -> get_int16_be bs
    | Int32_BE -> get_int32_be bs

(** [match_single_const bs p] matches a [const_pattern_type] element [p] against bitstream [bs] and returns true iff 
    the matched element is equivalent to [p]'s argument. Raises [Bitstream_length_exception] if the element is too long.*)
let match_single_const bs =
    function
    | ConstUInt8 i -> i = get_uint8 bs
    | ConstInt8 i -> i = get_int8 bs
    | ConstUInt16_LE i -> i = get_uint16_le bs
    | ConstInt16_LE i -> i = get_int16_le bs
    | ConstInt32_LE i -> i = get_int32_le bs
    | ConstUInt16_BE i -> i = get_uint16_be bs
    | ConstInt16_BE i -> i = get_int16_be bs
    | ConstInt32_BE i -> i = get_int32_be bs

exception Const_match_exception of int

let single_const_int_eval bs f i = if f bs = i then i else raise @@ Const_match_exception i

let match_single_const_int bs =
    let msc = single_const_int_eval bs in
    function
    | ConstUInt8 i -> msc get_uint8 i
    | ConstInt8 i -> msc get_int8 i
    | ConstUInt16_LE i -> msc get_uint16_le i
    | ConstInt16_LE i -> msc get_int16_le i
    | ConstInt32_LE i -> msc get_int32_le i
    | ConstUInt16_BE i -> msc get_uint16_be i
    | ConstInt16_BE i -> msc get_int16_be i
    | ConstInt32_BE i -> msc get_int32_be i

(** [match_single bs p] matches a [pattern_type] element [p] against bitstream [bs] and returns the result. Raises 
    [Const_match_exception] if a [const_pattern_type] could not be matched and [Bitstream_length_exception] if the
    [pattern_type] element is too long. *)
let match_single bs =
    function
    | C c -> match_single_const_int bs c
    | V v -> match_single_var bs v

(** [match_pattern bs l] matches the pattern [l], which is a list of [pattern_type] elements, on bitstream [bs] and 
    returns a list containing the matched elements. Values of [const_pattern_type] are also included in the returned list.
    Raises [Const_match_exception] if a [const_pattern_type] could not be matched and [Bitstream_length_exception] if the
    pattern [l] is too long. *)
let match_pattern bs = List.map @@ match_single bs

(** [match_const_pattern bs l] matches the pattern [l] of [const_pattern_type] elements on bitstream [bs] and returns true iff
    all elements in [l] are matched. Raises [Const_match_exception] if a [const_pattern_type] could not be matched and 
    [Bitstream_length_exception] if the pattern [l] is too long. *)
let match_const_pattern bs l = let open List in map (match_single_const bs) l |> exists (fun x -> x = false) |> not
