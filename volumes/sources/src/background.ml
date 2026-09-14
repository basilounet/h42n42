open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Vector


type tags_type = {
	river: string;
	hospital: string;
	grass: string;
}

let tags: tags_type = {
	river = "RIVER";
	hospital = "HOSPITAL";
	grass = "GRASS";
}

let river =
	let img_style = Printf.sprintf
		"position: absolute; width: 10vw; height: 100vh; user-select: none; \
		right: 0px; top: 0px; overflow: hidden; object-fit: fill;"
	in
	img
		~src:"static/images/river.png"
		~alt:(tags.river)
		~a:[a_class [tags.river]; 
			a_draggable false;
			a_style img_style]
		()

let hospital =
	let img_style = Printf.sprintf
		"position: absolute; width: 10vw; height: 100vh; user-select: none; \
		left: 0px; top: 0px; overflow: hidden; object-fit: fill;"
	in
	img
		~src:"static/images/hospital.png"
		~alt:(tags.hospital)
		~a:[a_class [tags.hospital]; 
			a_draggable false;
			a_style img_style]
		()

let grass =
	let img_style = Printf.sprintf
		"position: absolute; width: 100vw; height: 100vh; user-select: none; \
		left: 0vw; top: 0px; overflow: hidden; object-fit: fill;"
	in
	img
		~src:"static/images/grass.png"
		~alt:(tags.grass)
		~a:[a_class [tags.grass]; 
			a_draggable false;
			a_style img_style]
		()

let create body : vec2 = 
	let grass_node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img grass in
	Dom.appendChild body grass_node;
	Dom.appendChild body @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img river;
	Dom.appendChild body @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img hospital;
	vec2 (Int (grass_node##.width)) (Int (grass_node##.height))
