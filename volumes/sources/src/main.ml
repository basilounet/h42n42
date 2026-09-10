open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html

module Svg = Js_of_ocaml_tyxml.Tyxml_js.Svg

let generate_creet id =
	Creet.create 666 id 
	|> Creet.set_pos (id * 90, id * 40)
	|> Creet.set_radius @@ Float.of_int ((id + 1) * 10)
	|> Creet.set_color  @@ Printf.sprintf "#%X0000" @@ Utils.clamp (id * 20) 0 255

let rec generate_divs i = List.init i (fun i -> Creet.create_div (generate_creet i))


let page_content = generate_divs 8


let add_content body content =
	let dom_node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element content in
	Dom.appendChild body dom_node;
	()


let () =
	Dom_html.window##.onload := Dom_html.handler (fun _ ->
		let body = Dom_html.document##.body in
		List.iter (fun e -> add_content body e) page_content;
		Js._true
	)