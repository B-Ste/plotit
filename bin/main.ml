type image_processor_t =
    | CA
    | CM

let usage_msg = "poltit <command> -s <source> -o <output> [options]\n\
valid values for command: 'circle', 'circlematrix'"
let verbose = ref false
let input_file_r = ref None
let output_file_r = ref None
let image_processor = ref CA

let speclist = ref []
let speclist_main = let open Arg in 
    [("-verbose", Set verbose, "Output more information");
     ("-s", String (fun s -> input_file_r := Some s), "Source image-file");
     ("-o", String (fun s -> output_file_r := Some s), "Output SVG-file")]

let speclist_ca = let open Arg in
    [("-sc", Int (fun i -> Circle_approx.scale := i), "scale-down of original image");
     ("-it", Int (fun i -> Circle_approx.iterations := i), "number of attempts to draw circle");
     ("-r", Float (fun f -> Circle_approx.r := f), "radius of individual circles");
     ("-rs", Float (fun f -> Circle_approx.rs := f), "random-generator scaling (affects contrast)")]

let speclist_cm = let open Arg in
    [("-sc", Int (fun i -> Circle_matrix.vert_scale := i), "number of horizontal circles; aspect ratio is kept");
     ("-r", Float (fun f -> Circle_matrix.r := f), "maximum radius of circles");
     ("-rs", Float (fun f -> Circle_matrix.rs := f), "radius-scaling; affects contrast");
     ("-t", Float (fun f -> Circle_matrix.t := f), "minimum radius to draw; all radi below will be omitted")]

let anon_fun s = 
    (match s with
    | "circle" -> (image_processor := CA; speclist := speclist_main @ speclist_ca)
    | "circlematrix" -> (image_processor := CM; speclist := speclist_main @ speclist_cm)
    | _ -> (Printf.printf "Image processor %s is unknown.\n" s; exit 5)); ()

let verbose_print s = if !verbose then Printf.printf "%s" s else ()

let () =
    speclist := speclist_main;
    (try Arg.parse_argv_dynamic Sys.argv speclist anon_fun usage_msg
    with 
    | Arg.Help s -> Printf.printf "%s" s; exit 6
    | Arg.Bad s -> Printf.printf "%s" s; exit 7);

    if Option.is_none !input_file_r || Option.is_none !output_file_r then
        (Printf.printf "Both input and output files have to be given. See 'plotit -help' for more.\n";
        exit 4)
    else
    let input_file = Option.get !input_file_r in
    let output_file = Option.get !output_file_r in

    verbose_print (Printf.sprintf "Opening and decoding image at %s. " input_file);

    let img_r = ref None in
    (try img_r := Some (Png.decode_png input_file);
    with
        | Png.Invalid_png s -> 
            (Printf.printf "An error occured while opening and decoding %s: %s\n" input_file s; exit 1)
        | _ -> (Printf.printf "An unknown error occured while opening and decosind %s\n" input_file; exit 2));
    let img = Option.get !img_r in

    verbose_print "Done. \n";
    verbose_print (Printf.sprintf "width: %i, height %i\n" img.width img.height);
    verbose_print (Printf.sprintf "Opening SVG-file at %s. " output_file);

    let svg_r = ref None in
    (try svg_r := Some (Svg.open_svg output_file)
    with _ -> (Printf.printf "An error occured while opening an SVG-file at %s\n" output_file); exit 3);
    let svg = Option.get !svg_r in

    verbose_print "Done.\n";
    verbose_print "Processing image. ";

    let svg_p_r = ref svg in
    (match !image_processor with
    | CA -> svg_p_r := Circle_approx.circle_approx img svg
    | CM -> svg_p_r := Circle_matrix.circle_matrix img svg);

    verbose_print "Done.\n";
    verbose_print "Closing SVG-file. ";

    Svg.close !svg_p_r;

    verbose_print "Done.\n";;
