open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_lwt
open Vector

let () =
	Random.self_init ();
	Dom_html.window##.onload := Dom_html.handler (fun _ ->
		let body = Dom_html.document##.body in
		Sounds.setup_background_music body;
		Background.create body;
		Event_listeners.setup_keypresses body;
		Event_listeners.setup_click body;
		Event_listeners.track_cursor body;
		Menus.pause_menu body;
    Menus.on_retry := (fun () -> Behaviour.stop_game ());
    Menus.on_start := (fun () -> Behaviour.start_game body);
		Js._true
	)
