open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_lwt
open Vector

(* let generate_creet (id: int) =
	Creet.create 666 id 
	|> Creet.set_pos			@@ vec2 (Int (id * 7)) (Int (id * 5))
	|> Creet.set_radius 	@@ Float.of_int ((id + 1) * 3)
	|> Creet.set_color  	@@ Printf.sprintf "#%X0000" @@ Utils.clamp ((id + 1) * 20) 0 255
	|> Creet.set_rotation	@@ ((id * 36) |> Float.of_int |> from_angle ) *)

(* let page_content = List.init 11 (fun i -> Creet.create_div (generate_creet i)) *)

let generate_creet (id: int) =
	Creet.create 666 id 
	|> Creet.set_pos			@@ vec2 (Int (25 * id)) (Int (25 * id))
	|> Creet.set_radius		@@ Float.of_int (3)
	|> Creet.set_rotation	@@ (0 |> Float.of_int |> from_angle )

let add_content body content =
	Dom.appendChild body @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div content; 
	()

let rec creet_loop body (creet : Creet.creet) : unit Lwt.t =
	creet |> Creet.update body;
	Lwt.bind (Lwt_js.sleep 0.01) (fun () ->
	creet_loop body (Creet.move creet))

let () =
	Dom_html.window##.onload := Dom_html.handler (fun _ ->
		let body = Dom_html.document##.body in
		List.iter (add_content body) Background.background_elements;
		Lwt.async (fun () -> generate_creet 0 |> creet_loop body);
		Lwt.async (fun () -> generate_creet 1 |> creet_loop body);
		Lwt.async (fun () -> generate_creet 2 |> creet_loop body);
		Js._true
	)
