

class ['a] accessible (init: 'a) =
	object
		val mutable value: 'a = init
		method get (): 'a =
			value
		method set (new_val: 'a): unit =
			value <- new_val
	end


type simulation_variables = {
	width:	float;
	height:	float;
	troop:	Types.troop;
}


type creet_variables = {
	initial_radius:	float;
	border_margin:	float;
	panic:			float accessible;
	safe_space:		float accessible;
	stress:			float accessible;
	deviation:		float accessible;
	chase:			float accessible;
	infection:		int   accessible;
	death_timer:	float accessible;
}


let simulation: simulation_variables = {
	width	= 1000.;
	height	=  500.;
	troop	= Hashtbl.create 200;
}


let creet: creet_variables = {
	initial_radius	=                  1.  ;
	border_margin	=                  0.10;
	panic			= new accessible   0.05;
	safe_space		= new accessible 100.  ;
	stress			= new accessible   0.02;
	deviation		= new accessible   0.05;
	chase			= new accessible   0.05;
	infection		= new accessible   2   ; (* Is compared against a Random.int 100 *)
	death_timer		= new accessible  60.  ;
}

