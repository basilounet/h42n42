open Vector


type creet = {
	id:			int;
	radius: 	float;
	pos:		vec2;
	rotation:	vec2;
	color:		string;
	speed:		int;
	seed:		int;
	random:		Random.State.t;
}


val create_div	: creet		-> [> Html_types.div ] Js_of_ocaml_tyxml.Tyxml_js.Html.elt


val set_radius  : float     -> creet  -> creet 
val set_pos   	: vec2   	-> creet  -> creet 
val set_rotation: vec2		-> creet  -> creet 
val set_color 	: string    -> creet  -> creet
val set_speed 	: int       -> creet  -> creet


val create : int -> int -> creet

