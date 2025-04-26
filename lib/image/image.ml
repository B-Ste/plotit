type rgb = {r: int; g: int; b:int}
type image = {data: rgb array array; width: int; height: int}

(** [create_ppm i s] creates a ppm-file showing image [i] at location [s]. *)
let create_ppm i s = Out_channel.with_open_bin s (
    fun ch -> let open Printf in 
    fprintf ch "P6\n";
    fprintf ch "%i %i\n" i.width i.height;
    fprintf ch "%i\n" 255;
    for h = 0 to i.height - 1 do
        for w = 0 to i.width - 1 do
            output_byte ch i.data.(h).(w).r;
            output_byte ch i.data.(h).(w).g;
            output_byte ch i.data.(h).(w).b;
        done;
    done;
);;

(** [map f i] returns a new image with function [f] applied to every pixel of image [i]. *)
let map f i = {data = Array.init_matrix i.height i.width (
    fun y x -> f i.data.(y).(x)
); width = i.width; height = i.height};;

(** [grayscale p] gives out the grayscale of rgb-value [p]. *)
let grayscale p = 
    let open Float in
    let v = to_int (0.299 *. of_int p.r +. 0.587 *. of_int p.g +. 0.114 *. of_int p.b) in
    {r = v; g = v; b = v};;

exception Invalid_image_operation of string;;

(** [scale_down ~x ?y i] scale image [i] to width [~x] and height [?y]. If no [y] is given, the
    aspect ratio will be preserved. Raises [Invalid_image_operation] if [x] or [y] are larger than the
    original width or height, respectively. *)
let scale_down ~x:x ?y:y_opt i =
    let default = Float.to_int ((Float.of_int x /. Float.of_int i.width) *. Float.of_int i.height) in
    let y = Option.value ~default:default y_opt in
    if x > i.width || y > i.height 
        then raise @@ Invalid_image_operation "scale_down can not scale up."
    else
    let stride_x = Float.to_int (Float.of_int i.width /. Float.of_int x) in
    let stride_y = Float.to_int (Float.of_int i.height /. Float.of_int y) in
    {data = Array.init_matrix y x (
        fun y x -> 
            let rr = ref 0 in
            let gr = ref 0 in
            let br = ref 0 in
            for y_i = y * stride_y to (y + 1) * stride_y - 1 do
                for x_i = x * stride_x to (x + 1) * stride_x - 1 do
                    rr := !rr + i.data.(y_i).(x_i).r;
                    gr := !gr + i.data.(y_i).(x_i).g;
                    br := !br + i.data.(y_i).(x_i).b;
                done;
            done;
            let fac = stride_y * stride_x in
        {r = !rr / fac; g = !gr / fac; b = !br / fac}
    ); width = x; height = y};;
