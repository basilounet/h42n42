open Params

type statistics = {
	time_elapsed:	float accessible;
	score:			int accessible;
	alive:			int accessible;
	max_alive:		int accessible;
	dead:			int accessible;
	healthy:		int accessible;
	sick:			int accessible;
	mean:			int accessible;
	berserk:		int accessible;
	healed:			int accessible;
	contaminations:	int accessible;
	evolutions:		int accessible;
}

val stats: statistics

val update_max_alive:	unit -> unit
val change_creet_state:	Types.creet -> Types.creet_state -> unit
val add_creet: unit -> unit