open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Vector

module Svg = Js_of_ocaml_tyxml.Tyxml_js.Svg

type creet_state = Healthy | Sick | Mean | Berserk

type creet = {
	id:         int;
    state:      creet_state;
	radius:     float;
	pos:        vec2;
	rotation:   vec2;
	color:      string;
	speed:      int;
	seed:       int;
	random:     Random.State.t;
}

let create_div (creet : creet) =
    let diameter = creet.radius *. 2. in
    (* let angle = to_angle creet.rotation in *)
    (* Printf.printf "rot: %.1f, %.1f. to_angle: %.1f\n" creet.rotation.x creet.rotation.y angle; *)
    (* Printf.printf "to_angle: %.1f. from_angle rot: %.1f, %.1f\n" angle (from_angle angle).x (from_angle angle).y; *)
    let img_style =
        Printf.sprintf
          "position: absolute; width: %.1fvw; height: %.1fvh; \
           transform: translate(-50%%, -50%%) rotate(%.1fdeg); \
           left: %.1fvw; top: %.1fvh"
          (diameter *. 0.5) diameter (to_angle creet.rotation) creet.pos.x creet.pos.y
    in
    div ~a:[a_class ["creet " ^ (string_of_int creet.id)]] [
        img
            ~src:"static/images/creet_healthy.png"
            ~alt:("creet " ^ string_of_int creet.id)
            ~a:[a_style img_style] ();
    ]

let set_state (state: creet_state) (creet: creet) : creet = 
    match state with
    | Mean | Berserk when creet.state = Mean || creet.state = Berserk -> creet
    | _ -> {creet with
        state = state
        }

let set_radius (radius: float) (creet: creet) : creet = 
	{ creet with
		radius = radius;
	}

let set_pos (pos: vec2) (creet: creet): creet = 
	{ creet with
		pos = pos;
	}

let set_color (color: string) (creet: creet) : creet = 
	{ creet with
		color = color;
	}

let set_rotation (rotation: vec2) (creet: creet) : creet = 
	{ creet with
		rotation = rotation;
	}

let set_speed (speed: int) (creet: creet) : creet = 
	{ creet with
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
		speed = 10;
		radius = 1.;
		pos = vec2 (Float 0.) (Float 0.);
		color = "#831c1c"
	}
