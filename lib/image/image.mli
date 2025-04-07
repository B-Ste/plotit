type rgb = {r: int; g: int; b:int}
type image = {data: rgb array array; width: int; height: int}

val create_ppm : image -> string -> unit