open Vector
open Js_of_ocaml
open Js_of_ocaml_lwt


let js_to_int num = num |> Js.float_of_number |> int_of_float


let generate_creet (id: int) (pos: vec2) =
	Creet.create 666 id 
	|> Creet.set_pos		@@ pos
	|> Creet.set_radius		@@ 1.
	|> Creet.set_direction	@@ (from_angle 0.)

let rec creet_loop body (creet : Creet.creet) : unit Lwt.t =
	ignore @@ Creet.update body creet;
	Lwt.bind (Lwt_js.sleep 0.02) (fun () ->
		creet_loop body (Creet.move creet))

let setup_click target (w_size:vec2) =
	Lwt.async (fun () ->
		Lwt_js_events.clicks target (fun ev _handler ->
		let pos = vec2
			(Int (ev##.clientX |> js_to_int))
			(Int (ev##.clientY |> js_to_int)) in
		Lwt.async (fun () -> generate_creet (Utils.create_id ()) pos |> creet_loop target);
   		Lwt.return_unit
	)
  );
  ()

