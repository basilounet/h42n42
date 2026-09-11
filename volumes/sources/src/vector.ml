
type vec2 = {
	x: float;
	y: float
}


type vec_aux_t = Float of float | Int of int | Float_Tuple of (float * float) | Int_Tuple of (int * int) | None


let vec2_int (x: int) (y: int) : vec2 = {
	x = Float.of_int x;
	y = Float.of_int y
}


let vec2_float (x: float) (y: float) : vec2 = {
	x = x;
	y = y
}


let vec2 (x: vec_aux_t) (y: vec_aux_t) : vec2 = match (x, y) with
	| Float x, Float y		-> vec2_float x y
	| Int x, Int y			-> vec2_int x y
	| Float_Tuple x, _		-> vec2_float (fst x) (snd x)
	| Int_Tuple x, _		-> vec2_int (fst x) (snd x)
	| _, _					-> vec2_float 0. 0.


let add (v: vec2) (u: vec2) : vec2 = {
	x = v.x +. u.x;
	y = v.y +. u.y
}


let sub (v: vec2) (u: vec2) : vec2 = {
	x = v.x -. u.x;
	y = v.y -. u.y
}


let scale (a: float) (v: vec2) : vec2 = {
	x = v.x *. a;
	y = v.y *. a
}


let rotate (alpha: float) (v: vec2) : vec2 = {
	x = v.x *. cos(alpha) -. v.x *. sin(alpha);
	y = v.y *. cos(alpha) +. v.x *. sin(alpha)
}


let length2 (v: vec2): float =
	v.x *. v.x +. v.y *. v .y


let length (v: vec2): float = v
	|> length2
	|> sqrt


let normalise (v: vec2) : vec2 = v
	|> scale (1. /. length v)


let stretch (len: float) (v: vec2) : vec2 = v
	|> normalise
	|> scale len


let distance2 (v:vec2) (u: vec2): float = u
	|> sub v
	|> length2


let distance (v: vec2) (u: vec2): float = u
	|> sub v
	|> length


let cross_product (v: vec2) (u: vec2): float =
	v.x *. u.x +. v.y *. u.y


let to_angle (v: vec2): float =
	atan2 v.x v.y
	|> Float.mul (Float.pi /. 180.)


let from_angle (alpha: float): vec2 = {
	x = cos(alpha);
	y = sin(alpha)
}


let (++) = add

let (--) = sub

let ( >> ) = scale

let ( ~~ ) = rotate

let ( >=< ) = length2

let ( >==< ) = length

let ( !! ) = normalise

let ( >>= ) = stretch

let ( |--| ) = distance2 

let ( |-| ) = distance

let ( ** ) = cross_product
