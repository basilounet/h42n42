open Vector
open Js_of_ocaml

type creet_state = Healthy | Sick | Mean | Berserk

type creet = {
	id:			int;
    state:      creet_state;
	radius: 	float;
	pos:		vec2;
	direction:	vec2;
	color:		string;
	speed:		float;
	seed:		int;
	random:		Random.State.t;
}

val style_string : creet -> string

val update      : #Js_of_ocaml.Dom.node Js_of_ocaml.Js.t -> creet -> creet


val set_state   	: creet_state   -> creet  -> creet 
val set_radius		: float         -> creet  -> creet 
val set_pos   		: vec2   	    -> creet  -> creet 
val set_direction	: vec2		    -> creet  -> creet 
val set_color 		: string        -> creet  -> creet
val set_speed 		: float         -> creet  -> creet


val create  : int -> int -> creet

val move    : creet      -> creet
