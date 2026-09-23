open Js_of_ocaml
open Js_of_ocaml_lwt
open Vector

(* Size variables *)
let width				()		= (Background.get_wsize ()).x
let height				()		= (Background.get_wsize ()).y
let window_margin		()		= 0.1

(* Behaviour variables *)
let bound_panic			()		= 0.05
let eyesight_range		()		= 200.
let collision_range		()		= 100.
let collision_factor	()		= 0.02
let deviation_factor	()		= 0.05
let chase_factor		()		= 0.05 
let infection_factor	()		= 2		(* Is compared against a Random.int 100 *)
let time_to_die			()		= 60.

let start_mutex: Mutex.t ref	= ref @@ Mutex.create ()


(* Helper functions *)
let eyesight (creet: Creet.t): float =
	creet.radius *. eyesight_range ()

let eyesight2 (creet: Creet.t): float =
	(eyesight creet) *. (eyesight creet)

let avoidance (creet: Creet.t): float =
	creet.radius *. collision_range ()

let avoidance2 (creet: Creet.t): float =
	(avoidance creet) *. (avoidance creet)


let target_exists (troop: Troop.t) (target: int): bool =
	match (Hashtbl.find_opt troop.map target) with
	| None	-> false
	| _		-> true


let is_target_ill (troop: Troop.t) (target: int): bool =
	match (Hashtbl.find troop.map target).state with
	| Creet.Healthy -> false
	| _				-> true


(* Independent Behaviours *)
let river_contamination (creet: Creet.t): Creet.t =
	if Background.is_in_river creet.pos.x
	then
		Creet.be_contaminated creet
	else
		creet


let creet_contamination (troop: Troop.t) (creet: Creet.t): Creet.t =
	let can_infect (creet1: Creet.t) (creet2: Creet.t): bool =
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
			&& creet.pos |--| other_creet.pos < avoidance2 creet
		then
			if Random.int 100 < infection_factor ()
			then
				ignore @@ Creet.be_contaminated other_creet
			else
				()
	in

	Hashtbl.to_seq_values troop.map
	|> Seq.iter infect_creet;
	creet


let berserk_growth (creet: Creet.t): Creet.t =
	creet.radius <- creet.radius *. 1.001;
	if creet.radius > Creet.minimum_radius *. 4.
	then
		Creet.be_dead creet
	else
		creet


let mean_new_target (troop: Troop.t) (creet: Creet.t): Creet.t =
	let viable_targets = Hashtbl.to_seq_values troop.map
		|> Seq.filter (fun (c: Creet.t) -> c.state = Healthy && c.id <> creet.target)
	in
	let num_targets = Seq.length viable_targets in
	if num_targets > 0
	then begin
		let target = Seq.drop (Random.int num_targets) viable_targets
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
	if Unix.time () -. creet.time_sick > time_to_die ()
	then
		Creet.be_dead creet
	else
		creet


(* Brain Behaviours *)
let avoid_bounds (creet: Creet.t): Creet.t =
	let width_v		= width () in
	let height_v	= height () in
	let border		= min (width_v *. (window_margin ())) (height_v *. (window_margin ())) in
	let margin_v	= (border +. (2. *. creet.radius)) /. 2. in

	
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
		(Float_t (compute_force_dir (creet.pos.x) (margin_v) (width_v -. margin_v )))
		(Float_t (compute_force_dir (creet.pos.y) (margin_v) (height_v -. margin_v)))
	in


	let compute_force_len (force: vec2): float =
		(* TODO: Make it so that the straighter it goes into the wall, the more it turns -> [1; +inf[ *)
		0.
	in


	if length2 force > 0.
	then begin
		creet.direction <- (force
			|> scale ((compute_force_len force) +. bound_panic ())
			|> add creet.direction
		);
	creet
	end else
		creet


let avoid_creets (troop: Troop.t) (creet: Creet.t): Creet.t =
	let delta: vec2 ref = ref @@ vec2 (Vec_None) (Vec_None) in

	let correct_trajectory (other_creet: Creet.t): unit =
		if creet != other_creet then begin
			let distance = creet.pos |-| other_creet.pos in
			if distance < avoidance creet && distance > 0. then begin
				let push_dir = creet.pos |> sub other_creet.pos in
				let strength = ((avoidance creet) -. distance) /. (avoidance creet) in
				delta := push_dir |> stretch strength |> add !delta
			end
		end
	in
	
	let end_function_call (): Creet.t =
		creet
	in
	
	Hashtbl.to_seq_values troop.map
	|> Seq.iter correct_trajectory;
	
	if length2 !delta > 0.
	then begin
		let steer = !delta |> stretch (collision_factor ()) in
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


let rec chase_creet (troop: Troop.t) (creet: Creet.t): Creet.t =
	let chase_target () = 
		let target		= Hashtbl.find troop.map creet.target in
		let dist2		= target.pos |--| creet.pos in
		if dist2 < (avoidance2 target)
		then
			mean_new_target troop creet
		else begin
			let direction = creet.pos -- target.pos in
			creet.direction <- (direction
				|> stretch (chase_factor ())
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
let healthy_brain (creet: Creet.t) (troop: Troop.t): unit =
	ignore @@ ( creet
		|> avoid_bounds
		|> avoid_creets troop
		|> random_deviation
		|> Creet.advance
		|> river_contamination
	)


let sick_brain (creet: Creet.t) (troop: Troop.t): unit =
	ignore @@ ( creet
		|> avoid_bounds
		|> avoid_creets troop
		|> random_deviation
		|> creet_contamination troop
		|> Creet.advance
	)


let mean_brain (creet: Creet.t) (troop: Troop.t): unit =
	ignore @@ ( creet
		|> avoid_bounds
		|> chase_creet troop
		|> random_deviation
		|> creet_contamination troop
		|> ask_to_die
		|> Creet.advance
	)


let berserk_brain (creet: Creet.t) (troop: Troop.t): unit =
	ignore @@ ( creet
		|> avoid_bounds
		|> berserk_growth
		|> creet_contamination troop
		|> Creet.advance
	)


let select_behaviour (creet: Creet.t) (troop: Troop.t): unit = 
	match creet.state with
	| Creet.Healthy -> healthy_brain creet troop
	| Creet.Sick	-> sick_brain creet troop
	| Creet.Mean	-> mean_brain creet troop
	| Creet.Berserk	-> berserk_brain creet troop
	| Creet.Dead	-> ()


let rec run (creet: Creet.t) (troop: Troop.t) (body: #Dom.node Js.t): unit Lwt.t =
	ignore @@ Creet.update body creet;
	Lwt.bind (Lwt_js.sleep 0.) (fun () ->
		ignore @@ select_behaviour creet troop;
		if creet.state = Creet.Dead
		then begin
			Hashtbl.remove troop.map creet.id;
			Lwt.return ()
		end else
			run creet troop body
	)


let ready (creet: Creet.t) (troop: Troop.t) (body: #Dom.node Js.t): unit =
	(* let has_started (): bool =
		Mutex.try_lock !start_mutex
	in

	while has_started () = false do
		ignore @@ Lwt_js.sleep 0.1
	done;
	Mutex.lock !start_mutex;
	Mutex.unlock !start_mutex; *)
	ignore @@ run creet troop body

