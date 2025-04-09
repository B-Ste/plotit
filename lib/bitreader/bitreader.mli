type bitreader

val create_bitreader : int list -> bitreader
val read_bit : bitreader -> int
val read_byte : bitreader -> int
val read_bits : bitreader -> int -> int
val read_bytes : bitreader -> int -> int
