

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
