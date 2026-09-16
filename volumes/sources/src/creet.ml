open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Vector


module Svg = Js_of_ocaml_tyxml.Tyxml_js.Svg


type creet_state = Healthy | Sick | Mean | Berserk
let creete_state_str (state: creet_state) = match state with
	| state when state = Healthy	-> "healthy"
	| state when state = Sick		-> "sick"
	| state when state = Mean		-> "mean"
	| state when state = Berserk	-> "Berserk"
	| _ 							-> "What?"


type t = {
	id:					int;
	state:				creet_state;
	mutable radius:		float;
	mutable pos:		vec2;
	mutable direction:	vec2;
	mutable speed:		float;
	seed:				int;
	random:				Random.State.t;
}


let style_string (creet: t) : string = 
	let diameter = creet.radius *. 2. in
	Printf.sprintf
		"position: absolute; left: %.1fpx; top: %.1fpx;
		 width: %.1fvw; height: %.1fvw; user-select: none; \
		 transform: translate(-50%%, -50%%) rotate(%.1fdeg)"
		creet.pos.x creet.pos.y diameter diameter (to_angle creet.direction +. 90.)
		

let create_img (creet : t) =
    img
        ~src:"static/images/creets/creet_healthy.png"
        ~alt:(string_of_int creet.id)
        ~a:[a_class ["creet " ^ (string_of_int creet.id)]; 
			a_draggable false;
			a_style ""]
        ()


let creet_nodes : (int, Dom_html.imageElement Js.t) Hashtbl.t = Hashtbl.create 16

let get_or_create_node body (creet : t) : Dom_html.imageElement Js.t =
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
	creet


let set_state (state: creet_state) (creet: t) : t = 
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

let create (seed: int) (id: int) =
	Random.init (seed + id);
	{
		random = Random.get_state ();
		id = id;
		state = Healthy;
		seed = seed + id;
		direction = vec2 (Float_t 1.) (Float_t 0.);
		speed = 1.;
		radius = 1.;
		pos = vec2 (Float_t 0.) (Float_t 0.);
	}


let debug_creet (creet: t): unit =
	Printf.printf "creet %d (%s) pos:(%.2f; %.2f) radius: %.2f dir: (%.2f; %.2f) speed: %.2f\n"
		(creet.id)
		(creet.state |> creete_state_str)
		(creet.pos.x)
		(creet.pos.y)
		(creet.radius)
		(creet.direction.x)
		(creet.direction.y)
		(creet.speed)


let advance (creet: t): t = 
	debug_creet creet;
	creet.pos <- creet.direction |> stretch creet.speed |> add creet.pos;
	creet
