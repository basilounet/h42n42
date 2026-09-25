

class ['a] accessible (init: 'a) =
	object
		val mutable value: 'a = init
		method get (): 'a =
			value
		method set (new_val: 'a): unit =
			value <- new_val
		method add (op) (new_val: 'a): unit =
			value <- op value new_val 
	end


type simulation_variables = {
	width:			float;
	height:			float;
	hospital_end:	float;
	river_start:	float;
	delta_time:		float accessible;
	mouse_pos:		Types.vec2 accessible;
	grabbed_creet:	int accessible;
	troop:			Types.troop;
}


type creet_variables = {
	initial_radius:	float;
	initial_speed:	float;
	border_margin:	float;
	panic:			float accessible;
	safe_space:		float accessible;
	stress:			float accessible;
	deviation:		float accessible;
	chase:			float accessible;
	infection:		int	 accessible;
	death_timer:	float accessible;
}


let simulation_width = 10000.
let simulation: simulation_variables = {
	width			= simulation_width;
	height			= simulation_width /. 2.;
	hospital_end	= simulation_width *. 0.1;
	river_start		= simulation_width *. 0.9;
	delta_time		= new accessible 0.;
	mouse_pos		= new accessible @@ Vector.vec2 (Types.Vec_None) (Types.Vec_None);
	grabbed_creet	= new accessible (-1);
	troop			= Hashtbl.create 200;
}


let creet: creet_variables = {
	initial_radius	=									1.25;
	initial_speed	=										1000.;
	border_margin	=										0.10;
	panic			= 			new accessible	1.05;
	safe_space		=		new accessible	50.	;
	stress			= 		new accessible	0.04;
	deviation		= 		new accessible	0.05;
	chase			=				new accessible	0.05;
	infection		=			new accessible	2	 ; (* Is compared against a Random.int 100 *)
	death_timer		=		new accessible	60.	;
}

