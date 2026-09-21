open Vector
open Js_of_ocaml


type creet_state = Healthy | Sick | Mean | Berserk


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


val minimum_radius	: float


val style_string 	: t													-> string
val update			: #Js_of_ocaml.Dom.node Js_of_ocaml.Js.t	-> t	-> t

val create  		: int										-> int	-> t
val set_state   	: creet_state								-> t	-> t
val set_radius		: float      								-> t	-> t
val set_pos			: vec2   	 								-> t	-> t
val set_direction	: vec2		 								-> t	-> t
val set_speed 		: float      								-> t	-> t
val advance			: t													-> t
val debug_creet		: t													-> unit