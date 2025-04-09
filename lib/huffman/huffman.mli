type 'a huffmann_tree

val create_huffman_tree : 'a huffmann_tree
val insert : 'a -> 'a huffmann_tree -> int -> int -> 'a huffmann_tree
val decode : 'a huffmann_tree -> Bitreader.bitreader -> 'a
