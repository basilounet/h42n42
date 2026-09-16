open Js_of_ocaml
open Js_of_ocaml_lwt


(* Constant variables *)
val width:				unit									-> float
val height:				unit									-> float
val window_margin:		unit									-> float
val bound_panic:		unit									-> float
val eyesight_range:		unit									-> float
val collision_factor:	unit									-> float
val collision_range:	unit									-> float
val deviation_factor:	unit									-> float


(* Helper functions *)
val	eyesight:			Creet.t									-> float
val	eyesight2:			Creet.t									-> float
val	avoidance:			Creet.t									-> float
val	avoidance2:			Creet.t									-> float


(* Behaviour Related Functions *)
val start_mutex:		Mutex.t ref
val ready:				Creet.t	-> Troop.t	-> #Dom.node Js.t	-> unit
val run:				Creet.t	-> Troop.t	-> #Dom.node Js.t	-> unit Lwt.t

