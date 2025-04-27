open Svg
open Image

let scale = ref 300
let iterations = ref 20000
let r = ref 3.
let rs = ref 1.2

let circle_approx i svg =
    let img = if i.width > !scale then scale_down ~x:!scale i else i in
    let x = Float.pred (Float.of_int img.width) in
    let y = Float.pred (Float.of_int img.height) in
    Random.self_init ();
    for _ = 1 to !iterations do
        let x_r = Random.float x in
        let y_r = Random.float y in
        let p = img.data.(Float.to_int y_r).(Float.to_int x_r) in
        let brightness = Float.to_int 
            (0.2126 *. Float.of_int p.r +. 0.7152 *. Float.of_int p.g +. 0.0722 *. Float.of_int p.b) in
        if Float.to_int (Random.float 256. /. !rs) >= brightness then
            let c = create_circle x_r y_r !r in add svg c
        else
            ()
    done;
    svg

