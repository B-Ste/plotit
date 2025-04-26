let img = Png.decode_png "test3.png" in 
Image.create_ppm img "test.ppm";;
