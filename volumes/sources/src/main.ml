open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html

let generate_creet id =
	Creet.create 666 id 
	|> Creet.set_pos (id * 90, id * 40)
	|> Creet.set_radius 	@@ Float.of_int ((id + 1) * 10)
	|> Creet.set_color  	@@ Printf.sprintf "#%X0000" @@ Utils.clamp ((id + 1) * 20) 0 255
	|> Creet.set_rotation	@@ id * 20 


let page_content = List.init 8 (fun i -> Creet.create_div (generate_creet i))

let add_content body content =
	Dom.appendChild body @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div content; 
	()


let () =
	Dom_html.window##.onload := Dom_html.handler (fun _ ->
		let body = Dom_html.document##.body in
		(* add_content body (Create_background.background_elements.[0]); *)
		List.iter (add_content body) Create_background.background_elements;
		List.iter (add_content body) page_content;
		Js._true
	)