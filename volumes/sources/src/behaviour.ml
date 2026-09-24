open Js_of_ocaml
open Js_of_ocaml_lwt
open Vector


let game_tick: unit Lwt_condition.t	= Lwt_condition.create ()


let target_exists (troop: Types.troop) (target: int): bool =
	match (Hashtbl.find_opt troop target) with
	| None	-> false
	| _		-> true


let is_target_ill (troop: Types.troop) (target: int): bool =
	match (Hashtbl.find troop target).state with
	| Healthy 	-> false
	| _			-> true


(* Independent Behaviours *)
let river_contamination (creet: Creet.t): Creet.t =
	if Background.is_in_river creet.pos.x
	then
		Creet.be_contaminated creet
	else
		creet


let creet_contamination (neighbors: Creet.t list) (creet: Creet.t): Creet.t =
	let can_infect (creet1: Creet.t) (creet2: Creet.t): bool =
		match creet2.grabbed with
		| true		-> false
		| _			->
			match creet1.state with
			| Healthy	-> false
			| _			->
				match creet2.state with
				| Healthy	-> true
				| _			-> false
	in

	let infect_creet (other_creet: Creet.t): unit =
		if true
			&& other_creet != creet
			&& can_infect creet other_creet
			&& creet.pos |--| other_creet.pos < Creet.avoidance2 creet
		then
			if Random.int 100 < Params.creet.infection#get ()
			then
				ignore @@ Creet.be_contaminated other_creet
			else
				()
	in

	List.iter infect_creet neighbors;
	creet


let berserk_growth (creet: Creet.t): Creet.t =
	creet.radius <- creet.radius *. 1.001;
	if creet.radius > Params.creet.initial_radius *. 4.
	then
		Creet.be_dead creet
	else
		creet


let mean_new_target (troop: Types.troop) (creet: Creet.t): Creet.t =
	let viable_targets = Hashtbl.to_seq_values troop
		|> Seq.filter (fun (c: Creet.t) -> c.state = Healthy && c.id <> creet.target)
	in
	let num_targets = Seq.length viable_targets in
	if num_targets > 0
	then begin
		let target = viable_targets 
			|> Seq.drop (Random.int num_targets) 
			|> Seq.find (fun (c: Creet.t) -> true)
			|> Option.get
		in
		creet.target <- target.id;
		creet
	end else begin
		creet.target <- creet.id;
		creet
	end


let ask_to_die (creet: Creet.t): Creet.t =
	if Unix.time () -. creet.time_sick > Params.creet.death_timer#get ()
	then
		Creet.be_dead creet
	else
		creet


let grabbed (creet: Creet.t): unit =
	creet.pos <- Params.simulation.mouse_pos#get ()


(* Brain Behaviours *)
let avoid_bounds (creet: Creet.t): Creet.t =
	let width	= Params.simulation.width in
	let height	= Params.simulation.height in
	let border	= min (width *. Params.creet.border_margin) (height *. Params.creet.border_margin) in
	let margin	= (border +. (2. *. creet.radius)) /. 2. in

	
	let compute_force_dir (pos: float) (min_bound: float) (max_bound: float): float =
		if pos < min_bound
		then
			Utils.smoothstep @@ (min_bound -. pos) /. min_bound
		else if pos > max_bound
		then
			-.(Utils.smoothstep @@ (pos -. max_bound) /. (min_bound))
		else
			0.
	in
	let force = vec2
		(Float_t (compute_force_dir (creet.pos.x) (margin) (width -. margin )))
		(Float_t (compute_force_dir (creet.pos.y) (margin) (height -. margin)))
	in


	let compute_force_len (force: vec2): float =
		(* TODO: Make it so that the straighter it goes into the wall, the more it turns -> [1; +inf[ *)
		0.
	in


	if length2 force > 0.
	then begin
		creet.direction <- (force
			|> scale ((compute_force_len force) +. Params.creet.panic#get ())
			|> add creet.direction
		);
	creet
	end else
		creet


let avoid_creets (neighbors: Creet.t list) (creet: Creet.t): Creet.t =
	let delta: vec2 ref = ref @@ vec2 (Vec_None) (Vec_None) in

	let correct_trajectory (other_creet: Creet.t): unit =
		if creet != other_creet then begin
			let distance = creet.pos |--| other_creet.pos in
			if distance < Creet.avoidance2 creet && distance > 0. then begin
				let avoidance = Creet.avoidance2 creet in
				let push_dir = creet.pos |> sub other_creet.pos in
				let strength = ((avoidance) -. distance) /. (avoidance) *. (Params.simulation.delta_time#get ()) in
				delta := push_dir |> stretch strength |> add !delta
			end
		end
	in
	
	let end_function_call (): Creet.t =
		creet
	in
	
	List.iter correct_trajectory neighbors;
	if length2 !delta > 0.
	then begin
		let steer = !delta |> stretch (Params.creet.stress#get ()) in
		creet.direction <- (creet.direction |> add steer |> normalise);
		end_function_call ()
	end else
		end_function_call ()


let random_deviation (creet: Creet.t): Creet.t =
	let random_num = Random.int 800 in
	if random_num < 8
	then begin
		let rotation_const	= 15.0 in
		let speed_const		= 0.03 in
		let (angle_deg, speed_mult) =
			match random_num with
			| 0 -> ((  rotation_const), (1.00 +. speed_const))
			| 1 -> ((             0.0), (1.00 +. speed_const))
			| 2 -> ((-.rotation_const), (1.00 +. speed_const))
			| 3 -> ((  rotation_const), (1.00               ))
			| 4 -> ((-.rotation_const), (1.00               ))
			| 5 -> ((  rotation_const), (1.00 -. speed_const))
			| 6 -> ((             0.0), (1.00 -. speed_const))
			| 7 -> ((-.rotation_const), (1.00 -. speed_const))
			| _ -> ((             0.0), (1.00               ))
		in
		if angle_deg <> 0.0 then
			creet.direction <- (creet.direction |> rotate angle_deg);
		if speed_mult <> 1.0 then
			creet.speed <- creet.speed *. speed_mult;
		creet
	end else
		creet


let rec chase_creet (troop: Types.troop) (creet: Creet.t): Creet.t =
	let chase_target () = 
		let target		= Hashtbl.find troop creet.target in
		let dist2		= target.pos |--| creet.pos in
		if dist2 < (Creet.avoidance2 target)
		then
			mean_new_target troop creet
		else begin
			let direction = creet.pos -- target.pos in
			creet.direction <- (direction
				|> stretch (Params.creet.chase#get ())
				|> add creet.direction
				|> normalise
			);
			creet
		end
	in

	let new_target () =
		ignore @@ mean_new_target troop creet;
		chase_creet troop creet
	in

	let no_target () =
		creet
	in

	if true
		&& creet.target <> creet.id
		&& target_exists troop creet.target
		&& is_target_ill troop creet.target
	then
		new_target ()
	else
		match creet.target with
		| -1							-> new_target ()
		| target when target = creet.id	-> no_target ()
		| _								-> chase_target ()


(* Brain *)
let healthy_brain (creet: Creet.t): unit =
	let neighbors = Grid.possible_collisions creet in
	ignore @@ ( creet
		|> avoid_bounds
		|> avoid_creets neighbors
		|> random_deviation
		|> Creet.advance
		|> river_contamination
	)


let sick_brain (creet: Creet.t): unit =
	let neighbors = Grid.possible_collisions creet in
	ignore @@ ( creet
		|> avoid_bounds
		|> avoid_creets neighbors
		|> random_deviation
		|> creet_contamination neighbors
		|> Creet.advance
	)


let mean_brain (creet: Creet.t): unit =
	let neighbors = Grid.possible_collisions creet in
	ignore @@ ( creet
		|> avoid_bounds
		|> chase_creet Params.simulation.troop
		|> random_deviation
		|> creet_contamination neighbors
		|> ask_to_die
		|> Creet.advance
	)


let berserk_brain (creet: Creet.t): unit =
	let neighbors = Grid.possible_collisions creet in
	ignore @@ ( creet
		|> avoid_bounds
		|> berserk_growth
		|> creet_contamination neighbors
		|> Creet.advance
	)


let select_behaviour (creet: Creet.t): unit = 
	match Menus.is_pause () with
	| true -> ()
	| false -> 
	match creet.grabbed with
	| true			-> grabbed creet
	| _				->
	match creet.state with
	| Types.Healthy -> healthy_brain creet
	| Types.Sick	-> sick_brain creet
	| Types.Mean	-> mean_brain creet
	| Types.Berserk	-> berserk_brain creet
	| Types.Dead	-> ()


let rec run (creet: Creet.t) (body: #Dom.node Js.t): unit Lwt.t =
	ignore @@ Creet.update body creet;
	ignore @@ select_behaviour creet;
	Lwt.bind (Lwt_condition.wait game_tick) (fun _ ->
		if creet.state = Types.Dead
		then begin
			Hashtbl.remove Params.simulation.troop creet.id;
			Lwt.return ()
		end else
			run creet body
	)


let last_time: float ref	= ref @@ Unix.gettimeofday ()


open Js_of_ocaml_tyxml.Tyxml_js
open Js_of_ocaml_tyxml.Tyxml_js.Html


let fps_text = (div ~a:[a_id "fps"; a_class ["text"]; a_style "top: 73vh; left: 45vw"][txt @@ string_of_float (!last_time /. 60.)])
let fps_text_node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div fps_text


let already_setup = ref false


let one_time_setup_listeners () : unit =
	match !already_setup with
	| true -> ()
	| false -> already_setup := true; Dom.appendChild (Dom_html.document##.body) fps_text_node


let rec simulation_loop (): unit Lwt.t =
	let new_time = Unix.gettimeofday () in
	Params.simulation.delta_time#set (new_time -. !last_time);
	last_time := new_time;
	one_time_setup_listeners ();
	fps_text_node##.textContent := Js.some (Js.string (string_of_float @@ Float.floor @@ 1. /. (Params.simulation.delta_time#get ())));

	Grid.clear ();
	Hashtbl.to_seq_values Params.simulation.troop
	|> Seq.iter Grid.add;
	Lwt_condition.broadcast game_tick ();
	Lwt.bind (Lwt_js.sleep (1.0 /. 120.0)) (fun _ ->
		simulation_loop ()
	)

