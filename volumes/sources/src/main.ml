open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_lwt
open Vector


let generate_position (creet: Creet.t): vec2 =
	let prev_rand	= Random.get_state () in
	Random.set_state creet.random;
	let minx	= Params.simulation.width  *. Params.creet.border_margin in
	let miny	= Params.simulation.height *. Params.creet.border_margin in
	let randx	= minx +. Random.float (Params.simulation.width -. minx) in
	let randy	= miny +. Random.float (Params.simulation.height -. miny) in
	let pos		= vec2 (Float_t randx) (Float_t randy) in
	Random.set_state prev_rand;
	pos


let generate_dir (creet: Creet.t): vec2 =
	let prev_rand	= Random.get_state () in
	Random.set_state creet.random;
	let randx	= 0.5 -. (Random.float 1.) in
	let randy	= 0.5 -. (Random.float 1.) in
	let pos		= vec2 (Float_t randx) (Float_t randy) in
	Random.set_state prev_rand;
	pos


let spawn_creets (num: int): unit = 
	for index = 1 to num do
		let new_index = Utils.create_id () in
		let new_creet = Creet.create 666 new_index in
		new_creet.pos <- generate_position new_creet;
		new_creet.direction <- generate_dir new_creet;
		Hashtbl.add Params.simulation.troop new_index new_creet;
	done


let () =
	Dom_html.window##.onload := Dom_html.handler (fun _ ->
		let body = Dom_html.document##.body in
		Sounds.setup_background_music body;
		Background.create body;
		Event_listeners.setup_keypresses body;
		Event_listeners.setup_click body;
		Event_listeners.track_cursor body;
		(* Menus.pause_menu body; *)
		spawn_creets 150;
		Hashtbl.to_seq_values Params.simulation.troop
		|> Seq.iter (fun (creet: Creet.t) -> Lwt.async (fun () -> Behaviour.run creet body));
		Lwt.async (fun () -> Behaviour.simulation_loop ());
		Js._true
	)