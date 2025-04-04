type bitstream
type const_pattern_type = 
    | ConstUInt8 of int
    | ConstInt8 of int
    | ConstUInt16 of int
    | ConstInt16 of int
    | ConstInt32 of int
type var_pattern_type = 
    | UInt8
    | Int8
    | UInt16
    | Int16
    | Int32
type pattern_type = 
    | C of const_pattern_type
    | V of var_pattern_type

val open_bitstream : string -> bitstream

val match_pattern : bitstream -> pattern_type list -> int list
val match_single : bitstream -> pattern_type -> int
val match_single_const : bitstream -> const_pattern_type -> bool
