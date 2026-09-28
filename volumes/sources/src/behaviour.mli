open Js_of_ocaml
open Js_of_ocaml_lwt


(* Behaviour Related Functions *)
val game_tick: 										   unit Lwt_condition.t

val start_game: Dom_html.bodyElement Js.t -> unit
