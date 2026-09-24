

type t		= Types.grid
type cell	= Types.cell

val clear:					unit			-> unit
val add: 					Creet.t			-> unit
val possible_collisions:	Creet.t 		-> Creet.t list
val cell_of_ints:			int		-> int	-> cell
