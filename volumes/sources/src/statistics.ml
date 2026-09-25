open Params

type statistics = {
	time_elapsed:		float accessible;
	score:					int accessible;
	alive:					int accessible;
	max_alive:			int accessible;
	dead:						int accessible;
	healthy:				int accessible;
	sick:						int accessible;
	mean:						int accessible;
	berserk:				int accessible;
	healed:					int accessible;
	contaminations:	int accessible;
	evolutions:			int accessible;
}

let stats: statistics = {
	time_elapsed =	new accessible 0.;
	score =					new accessible 0;
	alive =					new accessible 0;
	max_alive =			new accessible 0;
	dead =					new accessible 0;
	healthy =				new accessible 0;
	sick =					new accessible 0;
	mean =					new accessible 0;
	berserk =				new accessible 0;
	healed =				new accessible 0;
	contaminations =new accessible 0;
	evolutions =		new accessible 0;
}

let update_max_alive () =
	stats.max_alive#set (max (stats.alive#get ()) (stats.max_alive#get ()));
	()

let add_creet_state op (state: Types.creet_state): unit = 
	match state with
	| Healthy ->				stats.healthy#add op 1
	| Sick ->						stats.sick#add op 1
	| Mean ->						stats.mean#add op 1
	| Berserk ->				stats.berserk#add op 1
	| Dead ->						()

let change_creet_state (creet: Types.creet) (new_state: Types.creet_state): unit = 
	add_creet_state (-) creet.state;
	match new_state with
	| Healthy ->				
		stats.healed#add	(+) 1; 
		stats.healthy#add	(+) 1
	| Sick ->						
		stats.contaminations#add	(+) 1; 
		stats.sick#add		(+) 1
	| Mean ->						
		stats.evolutions#add	(+) 1; 
		stats.mean#add		(+) 1
	| Berserk ->				
		stats.evolutions#add	(+) 1; 
		stats.berserk#add	(+) 1
	| Dead ->						
		stats.alive#add (-) 1;
		stats.dead#add	(+) 1

let add_creet () = 
	stats.alive#add (+) 1;
	add_creet_state (+) Healthy;
	update_max_alive ()
