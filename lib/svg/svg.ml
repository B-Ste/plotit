(** Type for handeling a SVG file. *)
type svg = SVG of out_channel

(** Type for the elements a SVG path is made up of. *)
type svg_path_element = 
    | MOVETO of float * float
    | MOVETO_REL of float * float
    | CLOSEPATH
    | LINETO of float * float
    | LINETO_REL of float * float

(** Type for SVG paths*)
type svg_path = SVG_PATH of svg_path_element list

(** [open_svg s] opens a new SVG file with name s and returns a handle for it. *)
let open_svg s = 
    let ch = open_out s in 
    output_string ch "<?xml version=\"1.0\" ?>\n";
    output_string ch "<svg xmlns=\"http://www.w3.org/2000/svg\">\n";
    SVG ch;;

(** [add_path svg p] adds the SVG path [p] to the SVG file [svg] by writing it into the file. *)
let add_path (SVG ch) (SVG_PATH el) = 
    let print_path_element = Printf.(function
        | MOVETO (x, y) -> fprintf ch "M %.6f %.6f " x y
        | MOVETO_REL (x, y) -> fprintf ch "m %.6f %.6f " x y
        | CLOSEPATH -> output_string ch "z "
        | LINETO (x, y) -> fprintf ch "L %.6f %.6f " x y
        | LINETO_REL (x, y) -> fprintf ch "l %.6f %.6f " x y)
    in let elems = List.rev el in
        output_string ch "<path d=\"";
        List.iter print_path_element elems;
        output_string ch "\" fill=\"none\" stroke=\"black\" stroke-width=\"1\" vector-effect=\"non-scaling-stroke\"/>\n";;

(** [close svg] closes the SVG file [svg]. Must be called to obtain a valid SVG file. *)
let close (SVG ch) = 
    output_string ch "</svg>"; 
    close_out ch;;

(** Creates a new SVG path and returns it. *)
let create_path = SVG_PATH [];;

(** [path_add p e] appends SVG path [p] with SVG path element [e]. *)
let path_add (SVG_PATH p) e = SVG_PATH (e :: p)

(** [path_moveto p x y] appends SVG path [p] with a moveto path element to coordinates [(x, y)]. *)
let path_moveto p x y = path_add p (MOVETO (x, y))

(** [path_moveto_rel p x y] appends SVG path [p] with a relative moveto path element with movement [(x, y)]. *)
let path_moveto_rel p x y = path_add p (MOVETO_REL (x, y))

(** [path_closepath p] closes SVG path [p]. *)
let path_closepath p = path_add p CLOSEPATH

(** [path_moveto p x y] appends SVG path [p] with a line path element to coordinates [(x, y)]. *)
let path_lineto p x y = path_add p (LINETO (x, y))

(** [path_moveto_rel p x y] appends SVG path [p] with a relative line path element with movement [(x, y)]. *)
let path_lineto_rel p x y = path_add p (LINETO_REL (x, y))
