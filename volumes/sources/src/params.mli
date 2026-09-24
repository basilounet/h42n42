

class ['a] accessible: 'a	->
	object
		method get: unit	-> 'a
		method set: 'a		-> unit
	end


type simulation_variables = {
	width:			float;
	height:			float;
	hospital_end:	float;
	river_start:	float;
	delta_time:		float accessible;
	mouse_pos:		Types.vec2 accessible;
	troop:			Types.troop;
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


val simulation:	simulation_variables
val creet:		creet_variables


