type bitstream
type const_pattern_type = 
    | ConstUInt8 of int
    | ConstInt8 of int
    | ConstUInt16_LE of int
    | ConstInt16_LE of int
    | ConstInt32_LE of int
    | ConstUInt16_BE of int
    | ConstInt16_BE of int
    | ConstInt32_BE of int
type var_pattern_type = 
    | UInt8
    | Int8
    | UInt16_LE
    | Int16_LE
    | Int32_LE
    | UInt16_BE
    | Int16_BE
    | Int32_BE
type pattern_type = 
    | C of const_pattern_type
    | V of var_pattern_type

val open_bitstream : string -> bitstream

val match_pattern : bitstream -> pattern_type list -> int list
val match_single : bitstream -> pattern_type -> int
val match_single_const : bitstream -> const_pattern_type -> bool
