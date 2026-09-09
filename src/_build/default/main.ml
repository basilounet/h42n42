open Js_of_ocaml


let () =
	let message = Js.string "Hello from jaksho!" in
	Dom_html.window##alert message