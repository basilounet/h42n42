let clamp (x: 'a) (minimum: 'a) (maximum: 'a): 'a = 
	min x maximum |> max minimum 



let create_id =
	let n = ref 0 in
	fun () ->
		let id = !n in
		if id = -1 then
		failwith "Counter reached max value.";
		incr n;
		id
