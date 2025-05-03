let vert_scale = ref 0
let r = ref 0.
let rs = ref 1.
let t = ref 1.

let circle_matrix img_i svg = 
    let img = Image.scale_down ~x:!vert_scale img_i in
    for y = 0 to img.height - 1 do
        for x = 0 to img.width - 1 do
            let b = Image.brightness img.data.(y).(x) in
            let rad_f = (!r *. !rs) /. b in
            let rad = if rad_f >= !r then !r else rad_f in
            if rad >= !t then 
                (let c = Svg.create_circle (Float.of_int x *. !r) (Float.of_int y *. !r) rad in
                Svg.add svg c;)
            else ()
        done;
    done;
    svg;;
