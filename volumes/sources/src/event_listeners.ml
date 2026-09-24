open Vector
open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js.Html


let js_to_int num = num |> Js.float_of_number |> int_of_float


let try_grab (): Creet.t option =
	let mouse_pos = Params.simulation.mouse_pos# get() in
	let is_creet_clicked (creet: Creet.t): bool =
		let size_square = creet.radius *. creet.radius *. 11000. in
		if creet.pos |--| mouse_pos < size_square
		then
			true
		else
			false
	in

	let fake_creet = Creet.create (-1) (-1) in
	fake_creet.pos <- mouse_pos;
	(Grid.possible_collisions fake_creet)
	|> List.find_opt is_creet_clicked


let setup_click target: unit =
	let released_grabbed_creet (): unit Lwt.t =
		ignore @@ (Params.simulation.grabbed_creet# get()
		|> Hashtbl.find Params.simulation.troop
		|> Creet.be_released);
		Lwt.return_unit
	in
	let grab_creet () =
		let creet: Creet.t option = try_grab () in
		match creet with
		| None	-> Lwt.return_unit
		| _		-> creet |> Option.get |> Creet.be_grabbed |> ignore |> Lwt.return
	in

	Lwt.async (fun () ->
		Lwt_js_events.clicks target (fun ev _handler ->
    match Menus.is_pause () with
    | true  -> Lwt.return_unit
    | false ->
		match Params.simulation.grabbed_creet#get () with
		| -1	-> grab_creet ()
		| _		-> released_grabbed_creet ()
	));
	()


let track_cursor target: unit =
	Lwt.async (fun () ->
		Lwt_js_events.mousemoves target (fun ev _handler ->
			let pos = vec2 (Float_t (ev##.clientX |> Js.float_of_number)) (Float_t (ev##.clientY |> Js.float_of_number)) in
			let winsize = Background.get_wsize () in
			let width	= winsize.x in
			let height	= winsize.y in
			let sim_x	= pos.x /. width  *. Params.simulation.width  in
			let sim_y	= pos.y /. height *. Params.simulation.height in
			let sim_pos = vec2 (Float_t sim_x) (Float_t sim_y) in
			Params.simulation.mouse_pos#set (sim_pos);
			Lwt.return_unit
	));
	()


let setup_keypresses target: unit =
	Lwt.async (fun () -> Lwt_js_events.keypresses target (fun ev _handler ->
		match ev##.keyCode with
    (* | 118 -> Sounds.play_sound_effect Sounds.SReproduction; Lwt.return_unit *)
		(* Enter or Space *)
		| 13 | 32 -> begin match !Menus.close_modal with
			| Some close_modal -> close_modal ()
			| None -> Sounds.play_menu "open"; Menus.pause_menu target
		end; Lwt.return_unit
		| key -> (*Printf.printf "keyCode: %d\n" key; *)Lwt.return_unit
	));
	()
