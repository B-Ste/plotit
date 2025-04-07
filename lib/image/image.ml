type rgb = {r: int; g: int; b:int}
type image = {data: rgb array array; width: int; height: int}

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
