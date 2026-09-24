open Js_of_ocaml
open Js_of_ocaml_lwt


(* Behaviour Related Functions *)
val game_tick: 										   unit Lwt_condition.t
val run:				Creet.t	-> #Dom.node Js.t	-> unit Lwt.t
val simulation_loop:	unit						-> unit Lwt.t

