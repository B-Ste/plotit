type svg = SVG of out_channel;;

type svg_path_element = 
    | MOVETO of float * float
    | MOVETO_REL of float * float
    | CLOSEPATH
    | LINETO of float * float
    | LINETO_REL of float * float

type svg_path = SVG_PATH of svg_path_element list;;

let open_svg s = 
    let ch = open_out s in 
    output_string ch "<?xml version=\"1.0\" ?>\n";
    output_string ch "<svg xmlns=\"http://www.w3.org/2000/svg\">\n";
    SVG ch;;

let add_path (SVG ch) (SVG_PATH el) = 
    let print_path_element = function
        | MOVETO (x, y) -> Printf.fprintf ch "M %.6f %.6f " x y
        | MOVETO_REL (x, y) -> Printf.fprintf ch "m %.6f %.6f " x y
        | CLOSEPATH -> output_string ch "z "
        | LINETO (x, y) -> Printf.fprintf ch "L %.6f %.6f " x y
        | LINETO_REL (x, y) -> Printf.fprintf ch "l %.6f %.6f " x y
    in let elems = List.rev el in
        output_string ch "<path d=\"";
        List.iter print_path_element elems;
        output_string ch "\" fill=\"none\" stroke=\"black\" stroke-width=\"1\" vector-effect=\"non-scaling-stroke\"/>\n";;

let close (SVG ch) = 
    output_string ch "</svg>"; 
    close_out ch;;

let create_path = SVG_PATH [];;

let path_moveto (SVG_PATH p) x y = SVG_PATH (MOVETO (x, y) :: p)
let path_moveto_rel (SVG_PATH p) x y = SVG_PATH (MOVETO_REL (x, y) :: p)
let path_closepath (SVG_PATH p) = SVG_PATH (CLOSEPATH :: p)
let path_lineto (SVG_PATH p) x y = SVG_PATH (LINETO (x, y) :: p)
let path_lineto_rel (SVG_PATH p) x y = SVG_PATH (LINETO_REL (x, y) :: p)
