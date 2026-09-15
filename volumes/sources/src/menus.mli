open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html


val close_modal : (unit -> unit) option ref
val is_pause: unit -> bool


val show_modal: #Js_of_ocaml.Dom.node Js_of_ocaml.Js.t -> 
	string -> 
	[< Html_types.div_content_fun > `Button `H2 ] 
	Js_of_ocaml_tyxml.Tyxml_js.Html.elt list -> string -> unit
