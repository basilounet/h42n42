open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_lwt
open Vector

let () =
	Dom_html.window##.onload := Dom_html.handler (fun _ ->
		let body = Dom_html.document##.body in
		let w_size = Background.create body in
		Event_listeners.setup_click body w_size;
		Js._true
	)
