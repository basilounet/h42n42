open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_lwt
open Vector


let () =
	let troop = spawn_creets 10 in
	Dom_html.window##.onload := Dom_html.handler (fun _ ->
		let body = Dom_html.document##.body in
		ignore @@ Background.create body;
		(* Mutex.lock !Behaviour.start_mutex; *)
		Hashtbl.to_seq_values troop.map
		|> Seq.iter (fun (creet: Creet.t) -> Behaviour.ready creet troop body);
		(* Mutex.unlock !Behaviour.start_mutex; *)
		(* Event_listeners.setup_click body w_size; *)
		Js._true
	)

