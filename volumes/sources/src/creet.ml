open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Vector

module Svg = Js_of_ocaml_tyxml.Tyxml_js.Svg

type creet_state = Healthy | Sick | Mean | Berserk

type creet = {
	id:		 int;
	state:	  creet_state;
	radius:	 float;
	pos:		vec2;
	rotation:   vec2;
	color:	  string;
	speed:	  float;
	seed:	   int;
	random:	 Random.State.t;
}

let style_string (creet: creet) : string = 
	let diameter = creet.radius *. 2. in
	(* let angle = to_angle creet.rotation in *)
	(* Printf.printf "rot: %.1f, %.1f. to_angle: %.1f\n" creet.rotation.x creet.rotation.y angle; *)
	(* Printf.printf "to_angle: %.1f. from_angle rot: %.1f, %.1f\n" angle (from_angle angle).x (from_angle angle).y; *)
	Printf.sprintf
		"position: absolute; width: %.1fvw; height: %.1fvw; \
		left: %.1fvw; top: %.1fvh; \
		transform: translate(-50%%, -50%%) rotate(%.1fdeg)"
		diameter diameter creet.pos.x creet.pos.y (to_angle creet.rotation +. 90.)
		

let create_div (creet : creet) =
	(* Printf.printf "create div rot: %.1f, %.1f. to_angle: %.1f\n" creet.rotation.x creet.rotation.y (to_angle creet.rotation); *)
	div ~a:[a_class ["creet " ^ (string_of_int creet.id)]] [
		img
			~src:"static/images/creet_healthy.png"
			~alt:(string_of_int creet.id)
			~a:[a_style ""] ();
	]


let creet_nodes : (int, Dom_html.divElement Js.t) Hashtbl.t = Hashtbl.create 16

let get_or_create_node body (creet : creet) : Dom_html.divElement Js.t =
    match Hashtbl.find_opt creet_nodes creet.id with
    | Some node -> node
    | None ->
        let elt = create_div creet in
        let node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div elt in
        Dom.appendChild body node;
        Hashtbl.add creet_nodes creet.id node;
        node

let update body (creet : creet) : unit =
	(* Printf.printf "pos: %.1f, %.1f\n" creet.pos.x creet.pos.y; *)
	let node = get_or_create_node body creet in
    node##.style##.cssText := Js.string @@ style_string creet


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

let set_rotation (rotation: vec2) (creet: creet) : creet = { creet with
		rotation = rotation;
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
		rotation = vec2 (Float 1.) (Float 0.);
		speed = 0.12;
		radius = 1.;
		pos = vec2 (Float 0.) (Float 0.);
		color = "#831c1c";
	}

let move (creet: creet) : creet =
	(* TODO : da heck is this rotate *)
	let new_rotation = to_angle creet.rotation |> ( +. ) 0.8 |> from_angle in
	(* let new_rotation = rotate 0.1 creet.rotation in *)
	(* let new_rotation = creet.rotation in *)
	{ creet with
		rotation = new_rotation;
		pos = creet.pos ++ (creet.speed >> new_rotation)
	}
