open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html


val close_modal : (unit -> unit) option ref
val is_pause: unit -> bool


val pause_menu: #Js_of_ocaml.Dom.node Js_of_ocaml.Js.t -> unit
