open Js_of_ocaml
open Js_of_ocaml_lwt


(* Behaviour Related Functions *)
val game_tick: 	unit Lwt_condition.t
val running:		bool ref

val start_game: Dom_html.bodyElement Js.t -> unit
val stop_game:	unit -> unit
