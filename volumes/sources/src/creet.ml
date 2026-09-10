open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html

module Svg = Js_of_ocaml_tyxml.Tyxml_js.Svg

type creet = {
	id: int;
    radius: float;
    pos: (int * int);
    color: string;
    speed: int;
    seed: int;
    random: Random.State.t;
}

let create_div (creet : creet) =
    let diameter = creet.radius *. 2. in
    let pos_to_string =
        "; left: " ^ (fst creet.pos |> string_of_int) ^ "px" ^
        "; top: "  ^ (snd creet.pos |> string_of_int) ^ "px"
    in
    let img_style =
        Printf.sprintf
          "position: absolute; width: %.1fpx; height: %.1fpx; \
           border-radius: 50%%; object-fit: cover; background: %s%s;"
          diameter diameter creet.color pos_to_string
    in
    div ~a:[a_class ["creet " ^ (string_of_int creet.id)]] [
        img
          ~src:"static/images/Creet.png"
          ~alt:("creet " ^ string_of_int creet.id)
          ~a:[a_style img_style]
          ();
    ]

(* let make_circle (creet: creet) =
    Svg.circle ~a:[
        Svg.a_cx (100., None);
        Svg.a_cy (100., None);
        Svg.a_r (creet.radius, None);
        (* Svg.a_x (`X (fst pos) , None); *)
        (* Svg.a_y (`Y (snd pos), None); *)
        Svg.a_fill (`Color (creet.color, None));
    ] [] *)

let set_radius (radius: float) (creet: creet) : creet = 
	{ creet with
		radius = radius;
	}

let set_pos (pos:(int * int)) (creet: creet): creet = 
	{ creet with
		pos = pos;
	}
let set_color (color: string) (creet: creet) : creet = 
	Printf.printf "Color: %s\n" color;
	{ creet with
		color = color;
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
		seed = seed + id;
		speed = 10;
		radius = 50.;
		pos = (0, 0);
		color = "#831c1c"
	}
