

type vec2 = {
	x: float;
	y: float
}
type vec_aux_t = Float_t of float | Int of int | Float_Tuple of (float * float) | Int_Tuple of (int * int) | Vec_None


type creet_state = Healthy | Sick | Mean | Berserk | Dead
type creet = {
	id:					int;
	mutable state:		creet_state;
	mutable radius:		float;
	mutable pos:		vec2;
	mutable direction:	vec2;
	mutable speed:		float;
	mutable target:		int;
	mutable time_sick:	float;
	mutable grabbed:	bool;
	seed:				int;
	random:				Random.State.t;
}


type troop = (int, creet) Hashtbl.t


type cell = creet list
type grid = {
	cols: int;
	rows: int;
	data: cell Array.t;
}