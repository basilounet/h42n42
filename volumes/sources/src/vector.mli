
type vec2 = {
	x: float;
	y: float
}


val vec2: 			float	-> float	-> vec2


val add: 			vec2	-> vec2		-> vec2
val sub: 			vec2	-> vec2		-> vec2
val scale:			float	-> vec2		-> vec2
val rotate:			float	-> vec2		-> vec2
val length2:		vec2	-> float
val length:			vec2	-> float
val normalise:		vec2	-> vec2
val stretch:		float	-> vec2		-> vec2
val distance2:		vec2	-> vec2		-> float
val distance:		vec2	-> vec2		-> float
val cross_product:	vec2	-> vec2		-> float
val angle:			vec2	-> float


val (++): 			float	-> float	-> vec2
val (--):			vec2	-> vec2 	-> vec2
val ( >> ):			vec2	-> vec2 	-> vec2
val ( ~~ ):			float	-> vec2 	-> vec2
val ( >=< ):		vec2	-> float
val ( >==< ):		vec2	-> float
val ( !! ):			vec2	-> vec2
val ( >>= ):		float	-> vec2 	-> vec2
val ( |--| ):		vec2	-> vec2 	-> float
val ( |-| ):		vec2	-> vec2 	-> float
val ( ** ):			vec2	-> vec2 	-> float

