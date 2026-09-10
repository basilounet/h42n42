let clamp (x: 'a) (minimum: 'a) (maximum: 'a): 'a = 
	min x maximum |> max minimum 
