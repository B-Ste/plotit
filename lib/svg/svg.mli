type svg
type svg_path
type svg_path_element = 
    | MOVETO of float * float
    | MOVETO_REL of float * float
    | CLOSEPATH
    | LINETO of float * float
    | LINETO_REL of float * float

val open_svg : string -> svg

val close : svg -> unit

val add_path : svg -> svg_path -> unit

val create_path : svg_path

val path_add : svg_path -> svg_path_element -> svg_path

val path_moveto : svg_path -> float -> float -> svg_path
val path_moveto_rel : svg_path -> float -> float -> svg_path
val path_closepath : svg_path -> svg_path
val path_lineto : svg_path -> float -> float -> svg_path
val path_lineto_rel : svg_path -> float -> float -> svg_path
