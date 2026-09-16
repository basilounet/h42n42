open Js_of_ocaml
open Js_of_ocaml_lwt
open Vector


(* Constant variables *)
let width				()	= 1904.
let height				()	= 938.
let window_margin		()	= 0.02
let bound_panic			()	= 0.5
let eyesight_range		()	= 10.
let collision_factor	()	= 5.
let collision_range		()	= 0.1
let deviation_factor	()	= 0.05


(* Helper functions *)
let eyesight (creet: Creet.t): float =
	creet.radius *. eyesight_range ()

let eyesight2 (creet: Creet.t): float =
	(eyesight creet) *. (eyesight creet)

let avoidance (creet: Creet.t): float =
	creet.radius *. collision_factor ()

let avoidance2 (creet: Creet.t): float =
	(avoidance creet) *. (avoidance creet)


(* Behaviour Modications *)
let avoid_bounds (creet: Creet.t): Creet.t =
	let width_v			= width () in
	let height_v		= height () in
	let width_margin	= width_v *. (window_margin ()) +. creet.radius in
	let height_margin	= height_v *. (window_margin ()) +. creet.radius in

	match creet.pos.x with
	| x when x < width_margin				-> creet.pos <- creet.pos ++ (vec2 (Float_t ((width_margin -. creet.pos.x) /. width_margin)) (Float_t 0.));	creet
	| x when x > width_v -. width_margin	-> creet.pos <- creet.pos -- (vec2 (Float_t ((creet.pos.x -. width_v -. width_margin) /. width_margin))(Float_t 0.)); creet
	| _ -> match creet.pos.y with	
	| y when y < height_margin				-> creet.pos <- creet.pos ++ (vec2 (Float_t 0.) (Float_t ((height_margin -. creet.pos.y) /. height_margin))); creet
	| y when y > height_v -. height_margin	-> creet.pos <- creet.pos -- (vec2 (Float_t 0.) (Float_t ((creet.pos.y -. height_v -. height_margin) /. height_margin))); creet
	| _ -> creet


let avoid_creets (troop: Troop.t) (creet: Creet.t): Creet.t =
	let delta: vec2 ref = ref @@ vec2 (Float_t 0.) (Float_t 0.) in
	let correct_trajectory (other_creet: Creet.t): unit =
		if creet == other_creet then ();
		let creet_distance = creet.pos |--| other_creet.pos in
		if creet_distance < avoidance2 creet
		then
			delta := !delta |> sub other_creet.pos;
		()
	in
	let end_function_call (): Creet.t =
		(* Mutex.unlock troop.mutex; *)
		creet
	in
	
	(* Mutex.lock troop.mutex; *)
	Hashtbl.to_seq_values troop.map
	|> Seq.iter correct_trajectory;
	if length2 !delta = 0.
	then
		end_function_call ()
	else begin
		delta := !delta
		|> stretch (collision_factor ())
		|> add creet.direction;
		creet.direction <- !delta;
		end_function_call ()
	end


let avoid_deviation (troop: Troop.t) (creet: Creet.t): Creet.t =
	let delta: vec2 ref	= ref @@ vec2 (Float_t 0.) (Float_t 0.) in
	let neighbor_num	= ref 0 in
	let correct_trajectory (other_creet: Creet.t): unit =
		if creet == other_creet then ();
		let creet_distance = creet.pos |--| other_creet.pos in
		if creet_distance < eyesight2 creet
		then begin
			delta := !delta ++ other_creet.direction;
			incr neighbor_num;
		end
	in
	let end_function_call (): Creet.t =
		(* Mutex.unlock troop.mutex; *)
		creet
	in
	
	(* Mutex.lock troop.mutex; *)
	Hashtbl.to_seq_values troop.map
	|> Seq.iter correct_trajectory;
	if !neighbor_num = 0 || length2 !delta = 0.
	then
		end_function_call ()
	else begin 
		delta := !delta
		|> stretch ((deviation_factor ()) /. (Float.of_int !neighbor_num))
		|> add creet.direction;
		creet.direction <- !delta;
		end_function_call ()
	end


let default (creet: Creet.t) (troop: Troop.t): unit =
	ignore @@ ( creet
	|> avoid_deviation troop 
	|> avoid_creets troop
	|> avoid_bounds
	);
	creet.direction <- normalise creet.direction;
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

