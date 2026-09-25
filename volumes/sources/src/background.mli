open Js_of_ocaml
open Vector


type tags_type = {
	river: string;
	hospital: string;
	grass: string;
}


val tags:													tags_type
val create:			#Js_of_ocaml.Dom.node Js_of_ocaml.Js.t	-> unit
val get_wsize: 		unit									-> vec2
val is_in_river:	float									-> bool
val is_in_hospital:	float									-> bool

(* val set_fps: unit -> unit *)
val update_stats: unit -> unit
