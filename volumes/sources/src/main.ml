open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_lwt
open Vector


let generate_position (creet: Creet.t): vec2 =
	let prev_rand	= Random.get_state () in
	Random.set_state creet.random;
	(* Printf.printf "%fx%f\n" (Behaviour.width ()) (Behaviour.height ()); *)
	let minx	= (Behaviour.width ())  *. (Behaviour.window_margin ()) in
	let miny	= (Behaviour.height ()) *. (Behaviour.window_margin ()) in
	let randx	= minx +. (Random.float ((Behaviour.width ()) -. minx)) in
	let randy	= miny +. (Random.float ((Behaviour.height ()) -. miny)) in
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


let spawn_creets (num: int): Troop.t = 
	let troop: Troop.t = {
		mutex	= Mutex.create ();
		map		= Hashtbl.create (num * 2)
	} in
	for index = 0 to num - 1 do
		let new_index = Utils.create_id () in
		let new_creet = Creet.create 666 new_index in
		new_creet.pos <- generate_position new_creet;
		new_creet.direction <- generate_dir new_creet;
		Hashtbl.add troop.map new_index new_creet;
	done; 
	troop


let test_behaviour () =
	Dom_html.window##.onload := Dom_html.handler (fun _ ->
		let body = Dom_html.document##.body in
		ignore @@ Background.create body;
		let troop = spawn_creets 10 in
		(* Mutex.lock !Behaviour.start_mutex; *)
		Hashtbl.to_seq_values troop.map
		|> Seq.iter (fun (creet: Creet.t) -> Behaviour.ready creet troop body);
		(* Mutex.unlock !Behaviour.start_mutex; *)
		Js._true
	)


let test_menus () =
	Dom_html.window##.onload := Dom_html.handler (fun _ ->
		let body = Dom_html.document##.body in
		ignore @@ Background.create body;
		(* Event_listeners.setup_click body w_size; *)
		Event_listeners.setup_keypresses body;
		Menus.pause_menu body;
		Js._true
	)


let () =
	test_behaviour ()
	(* test_menus () *)
