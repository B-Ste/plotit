let svg = Svg.open_svg "Test.svg" in
let p = Svg.create_path in
Svg.add_path svg (Svg.path_lineto (Svg.path_moveto p 10. 10.) 50. 50.);
Svg.close svg;;
