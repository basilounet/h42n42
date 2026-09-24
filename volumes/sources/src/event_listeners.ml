open Vector
open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js.Html


let js_to_int num = num |> Js.float_of_number |> int_of_float


let rec creet_loop body (creet : Creet.t) : unit Lwt.t =
	ignore @@ Creet.update body creet;
	Lwt.bind (Lwt_js.sleep 0.02) (fun () ->
		creet_loop body (Creet.advance creet))


let setup_click (target: #Dom_html.eventTarget Js.t) (w_size: vec2) =
	Lwt.async (fun () ->
		Lwt_js_events.clicks target (fun ev _handler ->
    match Menus.is_pause () with
    | true  -> Lwt.return_unit
    | false ->
		(* let pos = vec2 (Int (ev##.clientX |> js_to_int)) (Int (ev##.clientY |> js_to_int)) in *)

		Lwt.return_unit
	));
	()


let setup_keypresses target =
	Lwt.async (fun () -> Lwt_js_events.keypresses target (fun ev _handler ->
		match ev##.keyCode with
    | 118 -> Sounds.play_sound_effect Sounds.SReproduction; Lwt.return_unit
		(* Enter or Space *)
		| 13 | 32 -> begin match !Menus.close_modal with
			| Some close_modal -> close_modal ()
			| None -> Menus.pause_menu target
		end; Lwt.return_unit
		| key -> Printf.printf "keyCode: %d\n" key; Lwt.return_unit
	));
	()
