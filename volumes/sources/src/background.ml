open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html


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
		"position: absolute; width: 10vw; height: 100vh; \
		right: 0px; top: 0px; overflow: hidden; object-fit: fill;"
	in
 	div ~a:[a_class [ tags.river ]] [	
        img
        	~src:"static/images/River.png"
			~alt:(tags.river)
            ~a:[a_style img_style] ();
	]

let hospital =
	let img_style = Printf.sprintf
		"position: absolute; width: 10vw; height: 100vh; \
		left: 0px; top: 0px; overflow: hidden; object-fit: fill;"
	in
	div ~a:[a_class [ tags.hospital ]] [	
        img
        	~src:"static/images/Hospital.png"
			~alt:(tags.hospital)
            ~a:[a_style img_style] ();
	]

let grass =
		let img_style = Printf.sprintf
		"position: absolute; width: 80vw; height: 100vh; \
		left: 10vw; top: 0px; overflow: hidden; object-fit: fill;"
	in
	div ~a:[a_class [ tags.grass ]] [	
        img
        	~src:"static/images/Grass.png"
			~alt:(tags.grass)
            ~a:[a_style img_style] ();
	]

let background_elements = [hospital; grass; river]
