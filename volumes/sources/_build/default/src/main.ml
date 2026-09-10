open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html


let create_div i =
	div ~a:[a_class ["content " ^ (string_of_int i) ]] [
		h1 [txt ("title number " ^ (string_of_int i)) ];
		p [txt "This is the first content injected dynamically!"];
		a ~a:[a_href "https://ocaml.org"] [txt "First!"];
	]


let rec generate_divs i = List.init i create_div


let page_content = Create_background.background_elements


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