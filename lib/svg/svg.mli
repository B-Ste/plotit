type svg
type svg_element
type svg_path_element = 
    | MOVETO of float * float
    | MOVETO_REL of float * float
    | CLOSEPATH
    | LINETO of float * float
    | LINETO_REL of float * float

val open_svg : string -> svg

val close : svg -> unit

val add : svg -> svg_element -> unit

val create_path : svg_element
val create_circle : float -> float -> float -> svg_element

val path_add : svg_element -> svg_path_element -> svg_element

val path_moveto : svg_element -> float -> float -> svg_element
val path_moveto_rel : svg_element -> float -> float -> svg_element
val path_closepath : svg_element -> svg_element
val path_lineto : svg_element -> float -> float -> svg_element
val path_lineto_rel : svg_element -> float -> float -> svg_element
