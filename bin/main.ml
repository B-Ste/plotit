let img = Png.decode_png "test.png" in 
Image.create_ppm img "test.ppm";;
