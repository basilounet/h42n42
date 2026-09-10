open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html


type tags_type = {
	river: string;
	hospital: string
}
let tags: tags_type = {
	river = "RIVER";
	hospital = "HOSPITAL"
}


let river =
	div ~a:[a_class [ tags.river ]] [	
		(*Element du div*)
	]


let hospital =
	div ~a:[a_class [ tags.hospital ]] [	
		(*Element du div*)
	]


let background_elements = [river; hospital]

