open Svg
open Image

let scale = ref 300
let iterations = ref 20000
let r = ref 10.
let rs = ref 10.

let circle_approx i svg =
    let img = if i.width > !scale then scale_down ~x:!scale i else i in
    let x = Float.pred (Float.of_int img.width) in
    let y = Float.pred (Float.of_int img.height) in
    Random.self_init ();
    for _ = 1 to !iterations do
        let x_r = Random.float x in
        let y_r = Random.float y in
        let p = img.data.(Float.to_int y_r).(Float.to_int x_r) in
        let brightness = Image.brightness p in
        let rb = ((255. -. brightness) /. !rs) in
        if rb > !r then 
            let c = create_circle x_r y_r rb in add svg c
        else ()
    done;
    svg

