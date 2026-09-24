open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Vector
module Svg = Js_of_ocaml_tyxml.Tyxml_js.Svg


type t		= Types.creet
type state	= Types.creet_state


let string_of_state = function
	| Types.Healthy	-> "healthy"
	| Types.Sick	-> "sick"
	| Types.Mean	-> "mean"
	| Types.Berserk	-> "berserk"
	| Types.Dead	-> "dead"


let style_string (creet: t) : string = 
	let diameter = match creet.state with
		| Mean	-> creet.radius *. 2. *. (1. -. 0.15)
		| _		-> creet.radius *. 2.
	in
	let screen_x = creet.pos.x /. Params.simulation.width *. 100. in
	let screen_y = creet.pos.y /. Params.simulation.height *. 100. in
	Printf.sprintf "
		position: absolute; left: %fvw; top: %fvh;
		width: %.1fvw; height: %.1fvw; user-select: none;
		transform: translate(-50%%, -50%%) rotate(%.1fdeg)
	"	screen_x screen_y diameter diameter (to_angle creet.direction +. 90.)


let create_img (creet : t) =
    img
        ~src:"/static/images/creets/creet_healthy.png"
        ~alt:(string_of_int creet.id)
        ~a:[a_id (string_of_int creet.id); a_draggable false]
        ()


let creet_nodes : (int, Dom_html.imageElement Js.t) Hashtbl.t = Hashtbl.create 16


let get_or_create_node (body: #Dom.node Js.t) (creet : t) : Dom_html.imageElement Js.t =
    match Hashtbl.find_opt creet_nodes creet.id with
    | Some node -> node
    | None ->
        let elt = create_img creet in
        let node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img elt in
        Dom.appendChild body node;
        Hashtbl.add creet_nodes creet.id node;
        node

		
let update (body: #Dom.node Js.t) (creet : t) : t =
	let node = get_or_create_node body creet in
	node##.style##.cssText := Js.string @@ style_string creet;
	node##.src := Js.string @@ "static/images/creets/creet_" ^ (string_of_state creet.state) ^ ".png";
	creet


let be_mean (creet: t): t =
	creet.state		<- Mean;
	creet.target	<- -1;
	creet


let be_berserk (creet: t): t =
	creet.state <- Berserk;
	creet


let be_sick (creet: t): t =
	creet.state <- Sick;
	creet


let be_dead (creet: t): t =
	Printf.printf "i am dead at %.0f\n" creet.time_sick;
	creet.state <- Dead;
	let body = Dom_html.document##.body in
	let node = Hashtbl.find creet_nodes creet.id in
	Dom.removeChild body node;
	creet


let be_contaminated (creet: t): t =
	let random_num = Random.int 10 in
	creet.time_sick <- Unix.time ();
	creet |>
	match random_num with
	| 0	-> be_mean
	| 1 -> be_berserk
	| _ -> be_sick


let set_state (state: state) (creet: t) : t = 
	match state with
	| Mean | Berserk when creet.state = Mean || creet.state = Berserk -> creet
	| _ -> {creet with
		state = state
		}

let set_radius (radius: float) (creet: t) : t = { creet with
		radius = radius;
	}

let set_pos (pos: vec2) (creet: t): t = { creet with
		pos = pos;
	}

let set_direction (direction: vec2) (creet: t) : t = { creet with
		direction = direction;
	}

let set_speed (speed: float) (creet: t) : t = { creet with
		speed = speed;
	}

let create (seed: int) (id: int): t =
	let creet_state = match id with
		(* | 0 -> Types.Mean *)
		| _ -> Types.Healthy
	in
	Random.init (seed + id);
	{
		random = Random.get_state ();
		id = id;
		state = creet_state;
		seed = seed + id;
		direction = vec2 (Float_t 1.) (Float_t 0.);
		speed = 1.;
		target = -1;
		radius = Params.creet.initial_radius;
		pos = vec2 (Float_t 0.) (Float_t 0.);
		time_sick = -1.;
	}


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
	creet.radius *. Params.creet.safe_space#get ()

	
let avoidance2 (creet: t): float =
	(avoidance creet) *. (avoidance creet)


let advance (creet: t): t = 
	let creet_speed = match creet.state with
		| Healthy	-> creet.speed
		| _			-> creet.speed *. (1. -. 0.15)
	in
	creet.pos <- creet.direction |> stretch creet_speed |> add creet.pos;
	creet

	