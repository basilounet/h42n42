open Vector
open Js_of_ocaml
open Js_of_ocaml_lwt


val setup_click:		Dom_html.bodyElement Js.t	-> unit
val track_cursor:		Dom_html.bodyElement Js.t	-> unit
val setup_keypresses:	Dom_html.bodyElement Js.t	-> unit

