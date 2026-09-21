
type vec2 = {
	x: float;
	y: float
}


type vec_aux_t = Float_t of float | Int of int | Float_Tuple of (float * float) | Int_Tuple of (int * int) | Vec_None


let vec2_int (x: int) (y: int) : vec2 = {
	x = Float.of_int x;
	y = Float.of_int y
}


let vec2_float (x: float) (y: float) : vec2 = {
	x = x;
	y = y
}


let vec2 (x: vec_aux_t) (y: vec_aux_t) : vec2 = match (x, y) with
	| Float_t x, Float_t y	-> vec2_float x y
	| Int x, Int y			-> vec2_int x y
	| Float_Tuple xy, _		-> vec2_float (fst xy) (snd xy)
	| Int_Tuple xy, _		-> vec2_int (fst xy) (snd xy)
	| _						-> vec2_float 0. 0.


let add (v: vec2) (u: vec2) : vec2 = {
	x = v.x +. u.x;
	y = v.y +. u.y
}


let sub (u: vec2) (v: vec2) : vec2 = {
	x = v.x -. u.x;
	y = v.y -. u.y
}


let scale (a: float) (v: vec2) : vec2 = {
	x = v.x *. a;
	y = v.y *. a
}


let rotate (alpha: float) (v: vec2) : vec2 =
	let radians = alpha *. (Float.pi /. 180.) in {
	x = v.x *. cos(radians) -. v.y *. sin(radians);
	y = v.x *. sin(radians) +. v.y *. cos(radians)
}


let length2 (v: vec2): float =
	v.x *. v.x +. v.y *. v.y


let length (v: vec2): float = 
	Float.hypot v.x v.y


let normalise (v: vec2) : vec2 = v
	|> scale (1. /. (length v))


let stretch (len: float) (v: vec2) : vec2 = v
	|> normalise
	|> scale len


let distance2 (v:vec2) (u: vec2): float = u
	|> sub v
	|> length2


let distance (v: vec2) (u: vec2): float = u
	|> sub v
	|> length


let dot_product (v: vec2) (u: vec2): float =
	v.x *. u.x +. v.y *. u.y


let to_angle (v: vec2): float =
	atan2 v.y v.x |> ( *. ) (180. /. Float.pi)


let from_angle (degrees: float): vec2 = 
	let radians = degrees *. (Float.pi /. 180.) in
	{
	x = cos radians;
	y = sin radians
}


let string_of_vector (v: vec2): string =
	"(" ^ (string_of_float v.x) ^ ";" ^ (string_of_float v.y) ^ ")"


let ( ++ ) = add

let ( -- ) = sub

let ( >> ) = scale

let ( ~~ ) = rotate

let ( >=< ) = length2

let ( >==< ) = length

let ( !! ) = normalise

let ( >>= ) = stretch

let ( |--| ) = distance2 

let ( |-| ) = distance

let ( ** ) = dot_product
