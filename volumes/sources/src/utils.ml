

let clamp (x: 'a) (minimum: 'a) (maximum: 'a): 'a = 
	min x maximum |> max minimum 


let smoothstep (x: float): float =
	let x = Float.max 0.0 (Float.min 1.0 x) in
	x *. x *. (3.0 -. 2.0 *. x)


let create_id =
	let n = ref 0 in
	fun () ->
		let id = !n in
		if id = -1 then
		failwith "Counter reached max value.";
		incr n;
		id

let time_elapsed_format (): string = 
	let time_int = Statistics.stats.time_elapsed#get () |> int_of_float in
	let two_digits = function
		| str when String.length str < 2 -> "0" ^ (String.of_char str.[0])
		| str -> str
	in
	Printf.sprintf "%s:%s" 
		(two_digits @@ string_of_int @@ time_int / 60) 
		(two_digits @@ string_of_int @@ time_int mod 60)
