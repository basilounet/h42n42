open Js_of_ocaml
open Js_of_ocaml_lwt
open Vector


(* Constant variables *)
let width				()	= (Background.get_wsize ()).x
let height				()	= (Background.get_wsize ()).y
let window_margin		()	= 0.05
let bound_panic			()	= 0.05
let eyesight_range		()	= 10.
let collision_range		()	= 2.
let collision_factor	()	= 0.02
let deviation_factor	()	= 0.05


(* Helper functions *)
let eyesight (creet: Creet.t): float =
	creet.radius *. eyesight_range ()

let eyesight2 (creet: Creet.t): float =
	(eyesight creet) *. (eyesight creet)

let avoidance (creet: Creet.t): float =
	creet.radius *. collision_range ()

let avoidance2 (creet: Creet.t): float =
	(avoidance creet) *. (avoidance creet)


(* Behaviour Modications *)
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
	let delta: vec2 ref = ref @@ vec2 (Float_t 0.) (Float_t 0.) in

	let correct_trajectory (other_creet: Creet.t): unit =
		if creet != other_creet then begin
			let distance = creet.pos |-| other_creet.pos in
			if distance < avoidance creet && distance > 0.00001 then begin
				let push_dir = creet.pos |> sub other_creet.pos in
				let strength = ((avoidance creet) -. distance) /. (avoidance creet) in
				delta := push_dir |> stretch strength |> add !delta
			end
		end
	in
	
	let end_function_call (): Creet.t =
		(* Mutex.unlock troop.mutex; *)
		creet
	in
	
	(* Mutex.lock troop.mutex; *)
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
		Printf.printf "random: %d\n" random_num;
		creet
	end else
		creet


let default (creet: Creet.t) (troop: Troop.t): unit =
	ignore @@ ( creet
		|> avoid_bounds
		|> avoid_creets troop
		|> random_deviation
	);
	ignore @@ Creet.advance creet


let start_mutex: Mutex.t ref = ref @@ Mutex.create ()


let rec run (creet: Creet.t) (troop: Troop.t) (body: #Dom.node Js.t): unit Lwt.t =
	ignore @@ Creet.update body creet;
	Lwt.bind (Lwt_js.sleep 0.) (fun () ->
		ignore @@ default creet troop;
		run creet troop body;
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

