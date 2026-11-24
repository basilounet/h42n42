

type t		= Types.grid
type cell	= Types.cell

val clear:						unit							-> unit
val add: 						Creet.t							-> unit
(* val iter_neighbours_optimised:	(Creet.t -> unit)	-> Creet.t	-> unit *)
val iter_neighbours:	(Creet.t -> unit)	-> Creet.t	-> unit
val cell_of_ints:				int					-> int		-> cell
