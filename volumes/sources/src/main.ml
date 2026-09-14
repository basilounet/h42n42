open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_lwt
open Vector

let create_id =
  let n = ref 0 in
  fun () ->
    let id = !n in
    if id = -1 then
      failwith "Counter reached max value.";
    incr n;
    id

let js_to_int num = num |> Js.float_of_number |> int_of_float

let generate_creet (id: int) (pos: vec2) =
	Creet.create 666 id 
	|> Creet.set_pos		@@ pos
	|> Creet.set_radius		@@ 3.
	|> Creet.set_direction	@@ (from_angle 50.)

let rec creet_loop body (creet : Creet.creet) : unit Lwt.t =
	ignore @@ Creet.update body creet;
	Lwt.bind (Lwt_js.sleep 0.01) (fun () ->
	creet_loop body (Creet.move creet))

let setup_click_listener target (w_size:vec2) =
	Lwt.async (fun () ->
    Lwt_js_events.clicks target (fun ev _handler ->
		Printf.printf "x:%d, y: %d\n" (ev##.clientX |> js_to_int) (ev##.clientY |> js_to_int);
		Printf.printf "w:%f, h: %f\n" (w_size.x) (w_size.y);
		(* Printf.printf "test:%d\n" (target##.classList##.length |> js_to_int); *)

		let pos = vec2
			(Int (ev##.clientX |> js_to_int))
			(Int (ev##.clientY |> js_to_int)) in
		Lwt.async (fun () -> generate_creet (create_id ()) pos |> creet_loop target);
    Lwt.return_unit
    )
  );
  ()

let () =
	Dom_html.window##.onload := Dom_html.handler (fun _ ->
		let body = Dom_html.document##.body in
		let w_size = Background.create body in
		setup_click_listener body w_size;
		Js._true
	)
