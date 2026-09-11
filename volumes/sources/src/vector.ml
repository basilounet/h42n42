
type vec2 = {
	x: float;
	y: float
}


let vec2 (x: float) (y: float) : vec2 = {
	x = x;
	y = y
}


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


let angle (v: vec2): float =
	atan2 v.x v.y
	|> Float.mul (Float.pi /. 180.)


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
