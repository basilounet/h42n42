open Types
open Vector
open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js.Html


let js_to_int num = num |> Js.float_of_number |> int_of_float


let generate_creet (id: int) (pos: vec2) =
	Creet.create 666 id 
	|> Creet.set_pos		@@ pos
	|> Creet.set_radius		@@ 1.
	|> Creet.set_direction	@@ (from_angle 0.)


let rec creet_loop body (creet : creet) : unit Lwt.t =
	ignore @@ Creet.update body creet;
	Lwt.bind (Lwt_js.sleep 0.02) (fun () ->
		creet_loop body (Creet.move creet))


let setup_click target (w_size:vec2) =
	Lwt.async (fun () ->
		Lwt_js_events.clicks target (fun ev _handler ->
    match Menus.is_pause () with
    | true  -> Lwt.return_unit
    | false ->
		(* let pos: vec2 = vec2 (Int_Tuple (Dom_html.elementClientPosition target)) None in *)
		let pos = vec2 (Int (ev##.clientX |> js_to_int)) (Int (ev##.clientY |> js_to_int)) in
		(* Printf.printf "pos x: %f, y: %f\n" pos.x pos.y; *)
		Lwt.async (fun () -> generate_creet (Utils.create_id ()) pos |> creet_loop target);
		Lwt.return_unit
	));
	()

let setup_keypresses target =
	Lwt.async (fun () -> Lwt_js_events.keypresses target (fun ev _handler ->
		match ev##.keyCode with
		(* Enter or Space *)
		| 13 | 32 -> begin match !Menus.close_modal with
			| Some close_modal -> close_modal ()
			| None -> Menus.pause_menu target 
		end; Lwt.return_unit
		| key -> Printf.printf "keyCode: %d\n" key; Lwt.return_unit
	));
	()
