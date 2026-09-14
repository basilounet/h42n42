open Js_of_ocaml
open Vector


type tags_type = {
	river: string;
	hospital: string;
	grass: string;
}

val tags: tags_type

val create : #Js_of_ocaml.Dom.node Js_of_ocaml.Js.t -> vec2

(* val grass_node : #Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t option ref *)