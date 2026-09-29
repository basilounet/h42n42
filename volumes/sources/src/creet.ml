open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Vector
module Svg = Js_of_ocaml_tyxml.Tyxml_js.Svg


type t			= Types.creet
type state	= Types.creet_state


let string_of_state = function
	| Types.Healthy	-> "healthy"
	| Sick		-> "sick"
	| Mean		-> "mean"
	| Berserk	-> "berserk"
	| Dead		-> "dead"
	| Fake		-> "fake"


let style_string (creet: t) : string = 
	let size_factor = match creet.state with
		| Mean	-> (1. -. 0.15) *. 1.5
		| _		-> (1.) *. 1.5
	in
	let diameter = creet.radius *. 2. *. size_factor in
	let screen_x = creet.pos.x /. Params.simulation.width *. 100. in
	let screen_y = creet.pos.y /. Params.simulation.height *. 100. in
	(* Printf.printf "diamter: %.1f\n" diameter; *)
	Printf.sprintf "
		position: absolute; left: %fvw; top: %fvh;
		width: %.1fvw; height: %.1fvw; user-select: none;
		transform: translate(-50%%, -50%%) rotate(%.1fdeg); z-index: 2;
	"	screen_x screen_y diameter diameter (to_angle creet.direction +. 90.)


let create_img (creet : t) =
	img
		~src:"/static/images/creets/healthy.png"
		~alt:(string_of_int creet.id)
		~a:[a_id (string_of_int creet.id); a_draggable false]
		()


let get_or_create_node (body: #Dom.node Js.t) (creet : t) : Dom_html.imageElement Js.t =
	match creet.html with
	| Some node -> node
	| None ->
	let elt = create_img creet in
	let node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img elt in
	creet.html <- Some node;
	Dom.appendChild body node;
	node

		
let update (body: #Dom.node Js.t) (creet : t) : t =
	let node = get_or_create_node body creet in
	node##.style##.cssText := Js.string @@ style_string creet;
	node##.src := Js.string @@ "static/images/creets/" ^ (string_of_state creet.state) ^ ".png";
	creet


let be_mean (creet: t): t =
	match creet.state with
	| Berserk -> creet
	| _ ->
	Sounds.play_sound_effect Sounds.SEvolution;
	Statistics.change_creet_state creet Mean;
	creet.state		<- Mean;
	creet.target	<- -1;
	creet


let be_berserk (creet: t): t =
	match creet.state with
	| Berserk -> creet
	| _ ->
	Sounds.play_sound_effect Sounds.SEvolution;
	Statistics.change_creet_state creet Berserk;
	creet.state <- Berserk;
	creet


let be_sick (creet: t): t =
	match creet.state with
	| Sick -> creet
	| _ ->
	Sounds.play_sound_effect Sounds.SContamination;
	Statistics.change_creet_state creet Sick;
	creet.state <- Sick;
	creet

let be_dead (creet: t): t =
	Sounds.play_sound_effect Sounds.SDeath;
	Statistics.change_creet_state creet Dead;
	creet.state <- Dead;
	let body = Dom_html.document##.body in
	match creet.html with
	| Some node -> Dom.removeChild body node; creet
	| None -> creet


let be_contaminated (creet: t): t =
	let random_num = Random.State.int creet.random 10 in
	creet.death <- Params.creet.death_timer#get ();
	creet.mutation <- Params.creet.mutation_timer#get ();
	creet |>
	match random_num with
	| 0	-> be_mean
	| 1 -> be_berserk
	| _ -> be_sick


let be_healed (creet: t): t =
	Statistics.change_creet_state creet Healthy;
	creet.state <- Healthy;
	creet


let be_grabbed (creet: t): t =
	Params.simulation.grabbed_creet# set (creet.id);
	creet.held <- Params.creet.hold_timer#get ();
	creet.grabbed <- true;
	creet


let be_released (creet: t): t =
	Params.simulation.grabbed_creet# set (-1);
	creet.grabbed <- false;
	match creet.state with
	| Types.Sick	-> begin
		match Background.is_in_hospital creet.pos.x with
		| true -> 
		Sounds.play_sound_effect Sounds.SHealing;
		Statistics.stats.score#add (+) 200;
		be_healed creet
		| false ->creet
	end
	| _				-> creet


let create ?(fake = false) (seed: int) (id: int): t =
	if not fake then Statistics.add_creet ();
	Random.init (seed + id);
	{
		id =				id;
		state =			Healthy;
		seed =			seed + id;
		direction	= vec2 (Float_t 1.) (Float_t 0.);
		speed =			Params.creet.initial_speed#get ();
		target		= -1;
		radius		= Params.creet.initial_radius;
		pos			= vec2 (Float_t 0.) (Float_t 0.);
		death		= -1.;
		mutation	= -1.;
		held		= -1.;
		grabbed		= false;
		html =			None;
		random		= Random.get_state ();
	}


let generate_position (creet: t): vec2 =
	let prev_rand	= Random.get_state () in
	Random.set_state creet.random;
	let minx		= Params.simulation.width	*. Params.creet.border_margin in
	let miny		= Params.simulation.height *. Params.creet.border_margin in
	let randx		= minx +. Random.float (Params.simulation.width -. minx) in
	let randy		= miny +. Random.float (Params.simulation.height -. miny) in
	let pos			= vec2 (Float_t randx) (Float_t randy) in
	Random.set_state prev_rand;
	pos


let generate_dir (creet: t): vec2 =
	let prev_rand	= Random.get_state () in
	Random.set_state creet.random;
	let randx	= 0.5 -. (Random.float 1.) in
	let randy	= 0.5 -. (Random.float 1.) in
	let pos		= vec2 (Float_t randx) (Float_t randy) in
	Random.set_state prev_rand;
	pos


let debug_creet (creet: t): unit =
	Printf.printf "creet %d (%s) pos:(%.2f; %.2f) radius: %.2f dir: (%.2f; %.2f) speed: %.2f\n"
		(creet.id)
		(creet.state |> string_of_state)
		(creet.pos.x)
		(creet.pos.y)
		(creet.radius)
		(creet.direction.x)
		(creet.direction.y)
		(creet.speed)


let avoidance (creet: t): float =
	creet.radius *. Params.creet.safe_space

	
let avoidance2 (creet: t): float =
	(avoidance creet) *. (avoidance creet)


let advance (creet: t): t = 
	let creet_speed = match creet.state with
		| Healthy	-> creet.speed
		| _			-> creet.speed *. (1. -. 0.15)
	in
	creet.pos <- creet.direction |> stretch (creet_speed *. Params.simulation.delta_time#get ()) |> add creet.pos;
	creet
