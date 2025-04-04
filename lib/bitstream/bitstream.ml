(** Type for handeling bitstreams. *)
type bitstream = {data: bytes; mutable pos: int; length: int}

(** Type to represent constant values in bitstream. *)
type const_pattern_type = 
    | ConstUInt8 of int
    | ConstInt8 of int
    | ConstUInt16 of int
    | ConstInt16 of int
    | ConstInt32 of int

(** Type to represent variable values in bitstream. *)
type var_pattern_type = 
    | UInt8
    | Int8
    | UInt16
    | Int16
    | Int32

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

let get f l bs =
    if bs.pos + l <= bs.length then
        let i = f bs.data bs.pos in 
        bs.pos <- bs.pos + l;
        i
    else raise Bitstream_length_exception;;

let get_uint8 = get Bytes.get_uint8 1

let get_int8 = get Bytes.get_int8 1

let get_uint16 = get Bytes.get_uint16_ne 2

let get_int16 = get Bytes.get_int16_ne 2

let get_int32 bs = get Bytes.get_int32_ne 4 bs |> Int32.to_int

let match_single_var bs =
    function
    | UInt8 -> get_uint8 bs
    | Int8 -> get_int8 bs
    | UInt16 -> get_uint16 bs
    | Int16 -> get_int16 bs
    | Int32 -> get_int32 bs

(** [match_single_const bs p] matches a [const_pattern_type] element [p] against bitstream [bs] and returns true iff 
    the matched element is equivalent to [p]'s argument. Raises [Bitstream_length_exception] if the element is too long.*)
let match_single_const bs =
    function
    | ConstUInt8 i -> i = get_uint8 bs
    | ConstInt8 i -> i = get_int8 bs
    | ConstUInt16 i -> i = get_uint16 bs
    | ConstInt16 i -> i = get_int16 bs
    | ConstInt32 i -> i = get_int32 bs

exception Const_match_exception of int

let single_const_int_eval bs f i = if f bs = i then i else raise @@ Const_match_exception i

let match_single_const_int bs =
    let msc = single_const_int_eval bs in
    function
    | ConstUInt8 i -> msc get_uint8 i
    | ConstInt8 i -> msc get_int8 i
    | ConstUInt16 i -> msc get_uint16 i
    | ConstInt16 i -> msc get_int16 i
    | ConstInt32 i -> msc get_int32 i

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
