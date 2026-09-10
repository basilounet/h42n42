open Js_of_ocaml


let () =
	let message = Js.string "Hello from 42!" in
	Dom_html.window##alert message