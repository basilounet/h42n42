open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Vector

module Svg = Js_of_ocaml_tyxml.Tyxml_js.Svg

type creet_state = Healthy | Sick | Mean | Berserk

type creet = {
	id:			int;
	state:		creet_state;
	radius:		float;
	pos:		vec2;
	direction:	vec2;
	color:		string;
	speed:		float;
	seed:		int;
	random:		Random.State.t;
}

let style_string (creet: creet) : string = 
	let diameter = creet.radius *. 2. in
	Printf.sprintf
		"user-select: none; position: absolute; left: %.1fpx; top: %.1fpx;
		width: %.1fvw; height: %.1fvw; \
		transform: translate(-50%%, -50%%) rotate(%.1fdeg)"
		creet.pos.x creet.pos.y diameter diameter (to_angle creet.direction +. 90.)
		

let create_img (creet : creet) =
    img
        ~src:"static/images/creet_healthy.png"
        ~alt:(string_of_int creet.id)
        ~a:[a_class ["creet " ^ (string_of_int creet.id)]; 
			a_draggable false;
			a_style ""]
        ()


let creet_nodes : (int, Dom_html.imageElement Js.t) Hashtbl.t = Hashtbl.create 16

let get_or_create_node body (creet : creet) : Dom_html.imageElement Js.t =
    match Hashtbl.find_opt creet_nodes creet.id with
    | Some node -> node
    | None ->
        let elt = create_img creet in
        let node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img elt in
        Dom.appendChild body node;
        Hashtbl.add creet_nodes creet.id node;
        node

let update body (creet : creet) : creet =
	let node = get_or_create_node body creet in
    node##.style##.cssText := Js.string @@ style_string creet;
	creet


let set_state (state: creet_state) (creet: creet) : creet = 
	match state with
	| Mean | Berserk when creet.state = Mean || creet.state = Berserk -> creet
	| _ -> {creet with
		state = state
		}

let set_radius (radius: float) (creet: creet) : creet = { creet with
		radius = radius;
	}

let set_pos (pos: vec2) (creet: creet): creet = { creet with
		pos = pos;
	}

let set_color (color: string) (creet: creet) : creet = { creet with
		color = color;
	}

let set_direction (direction: vec2) (creet: creet) : creet = { creet with
		direction = direction;
	}

let set_speed (speed: float) (creet: creet) : creet = { creet with
		speed = speed;
	}

let create (seed: int) (id: int) =
	Random.init (seed + id);
	{
		random = Random.get_state ();
		id = id;
		state = Healthy;
		seed = seed + id;
		direction = vec2 (Float 1.) (Float 0.);
		speed = 0.12;
		radius = 1.;
		pos = vec2 (Float 0.) (Float 0.);
		color = "#831c1c";
	}

let move (creet: creet) : creet =
	(* TODO : da heck is this rotate *)
	let new_direction = to_angle creet.direction |> ( +. ) 0.8 |> from_angle in
	(* let new_direction = rotate 0.1 creet.direction in *)
	(* let new_direction = creet.direction in *)
	{ creet with
		direction = new_direction;
		pos = creet.pos ++ (creet.speed >> new_direction)
	}
