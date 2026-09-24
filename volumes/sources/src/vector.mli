

type vec_aux_t	= Types.vec_aux_t
type vec2		= Types.vec2


val vec2: 				vec_aux_t	-> vec_aux_t		-> vec2
val add: 				vec2		-> vec2				-> vec2
val sub: 				vec2		-> vec2				-> vec2
val scale:				float		-> vec2				-> vec2
val rotate:				float		-> vec2				-> vec2
val length2:			vec2							-> float
val length:				vec2							-> float
val normalise:			vec2							-> vec2
val stretch:			float		-> vec2				-> vec2
val distance2:			vec2		-> vec2				-> float
val distance:			vec2		-> vec2				-> float
val dot_product:		vec2		-> vec2				-> float
val to_angle:			vec2							-> float
val from_angle:			float							-> vec2
val string_of_vector:	vec2							-> string





val (++): 				vec2		-> vec2				-> vec2
val (--):				vec2		-> vec2 			-> vec2
val ( >> ):				float		-> vec2 			-> vec2
val ( ~~ ):				float		-> vec2 			-> vec2
val ( >=< ):			vec2							-> float
val ( >==< ):			vec2							-> float
val ( !! ):				vec2							-> vec2
val ( >>= ):			float		-> vec2 			-> vec2
val ( |--| ):			vec2		-> vec2 			-> float
val ( |-| ):			vec2		-> vec2 			-> float
val ( ** ):				vec2		-> vec2 			-> float

