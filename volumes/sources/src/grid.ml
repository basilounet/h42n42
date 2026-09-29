open Vector


type t		= Types.grid
type cell	= Types.cell


let safe_space (): float =
	Params.creet.safe_space *. Params.creet.initial_radius *. 4.


let cell_from_size ~(safe_space: float) (size: float): int =
	safe_space
	|> Float.div size 
	|> Float.ceil
	|> Float.to_int 


let cell_has_creet (creet: Creet.t) (cell: cell): bool = 
	cell |> List.exists (fun (c: Creet.t) -> c.id = creet.id)


let create (): t = 
	let safe_space	= safe_space () in
	let cols		= cell_from_size ~safe_space Params.simulation.width in
	let rows		= cell_from_size ~safe_space Params.simulation.height in
	let data		= Array.init (cols * rows) (fun _ -> []) in
	Printf.printf "Size of grid: %d x %d\n" cols rows;
	{ cols; rows; data; }


let grid: t = create ()


let clamp_x (x: int): int =
	Utils.clamp x (0) (grid.cols - 1)


let clamp_y (y: int): int =
	Utils.clamp y (0) (grid.rows - 1)


let index_of_ints (x: int) (y: int): int =
	(clamp_y y) * grid.cols + (clamp_x x)


let index_of_creet (creet: Creet.t): int =
	let safe_space = safe_space () in
	let cell_x = cell_from_size ~safe_space creet.pos.x in
	let cell_y = cell_from_size ~safe_space creet.pos.y in
	index_of_ints (cell_x) (cell_y)


let cell_of_int (index: int): cell = 
	Array.get grid.data (Utils.clamp index 0 (Array.length grid.data - 1))


let cell_of_ints (x: int) (y: int): cell = 
	cell_of_int (index_of_ints x y)


let cell_of_creet (creet: Creet.t): cell = 
	cell_of_int (index_of_creet creet)


let clear (): unit =
	let safe_space	= safe_space () in
	let cols		    = cell_from_size ~safe_space Params.simulation.width in
	let rows		    = cell_from_size ~safe_space Params.simulation.height in
	Array.fill grid.data 0 (cols * rows) [];
	()


let add (creet: Creet.t): unit =
	let expected_index = index_of_creet creet in
	let creet_cell = cell_of_creet creet in
	grid.data.(expected_index) <- creet :: creet_cell;
	()


let iter_neighbours_optimised (f: Creet.t -> unit) (creet: Creet.t): unit =
	let safe_space = safe_space () in
	let cell_x = cell_from_size ~safe_space creet.pos.x in
	let cell_y = cell_from_size ~safe_space creet.pos.y in

	let check_radius = (creet.radius /. Params.creet.initial_radius) |> Float.ceil |> Float.to_int in
	
	let clamp_x (x: 'a) = Utils.clamp x 0 (grid.cols - 1) in
	let clamp_y (y: 'a) = Utils.clamp y 0 (grid.rows - 1) in

	for iter_y = clamp_y (cell_y - check_radius)
	to clamp_y (cell_y + check_radius)
	do
		for iter_x = clamp_x (cell_x - check_radius)
		to clamp_x (cell_x + check_radius)
		do
			(cell_of_ints (iter_x) (iter_y))
			|> List.iter (fun (other: Creet.t) -> 
				if other.id <> creet.id && not other.grabbed then f other
			)
		done;
	done

let iter_neighbours (f: Creet.t -> unit) (creet: Creet.t): unit =
  match Params.simulation.is_optimised#get () with
  | true  -> iter_neighbours_optimised f creet
  | false ->
  Params.simulation.troop |> Hashtbl.to_seq_values |> Seq.iter 
		(fun (other: Creet.t) -> if other.id <> creet.id && not other.grabbed then f other)

