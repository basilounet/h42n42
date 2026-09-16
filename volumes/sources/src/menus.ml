open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt
open Js_of_ocaml_tyxml.Tyxml_js.Html


let close_modal : (unit -> unit) option ref = ref None

let is_pause (): bool = match !close_modal with
	| Some _	-> true
	| None 		-> false


let overlay_style =
	"position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; \
	 background: rgba(0,0,0,0.6); display: flex; align-items: center; \
	 justify-content: center; z-index: 1000;"

let pause_title_style = 
	"position: absolute; top: -15vh; left: 0vw; \
	 width: 20vw; height: 10vh; background-image: url('static/images/UI/pause_title.png'); \
	 background-repeat: no-repeat; background-size: 100% 100%; \
	 transform: translate(25%)"

let stats_style = 
	"position: absolute; top: 9vh; left: 32vw; \
	 width: 30vw; height: 60vh; background-image: url('static/images/UI/statistics.png'); \
	 background-repeat: no-repeat; background-size: 100% 100%;"

let stats_title_style = 
	"position: absolute; top: 0vh; left: 32vw; \
	 width: 20vw; height: 10vh; background-image: url('static/images/UI/stats_title.png'); \
	 background-repeat: no-repeat; background-size: 100% 100%; \
	 transform: translate(25%)"

let pause_style =
	"position: absolute; top: 55vh; left: 50vw; \
	 width: 30vw; height: 70vh; background-image: url('static/images/UI/pause.png'); \
	 background-repeat: no-repeat; background-size: 100% 100%; \
	 transform: translate(-50%, -50%); text-align: center;"

let button_style =
	"position: absolute; top: 10vh; left: 50%; width: 15cvw; height: 5vh; \
	 background: none; border: none; cursor: pointer; \
	 background-image: url('static/images/UI/button_normal.png'); \
	 background-repeat: no-repeat; background-size: 100% 100%; \
	 transform: translate(-50%, -50%)"

let pause_menu body : unit =
	let close_button =
		button ~a:[a_class ["button_style"]] [txt "Go back to Game"]
	in
	let modal_box =
		div ~a:[a_style pause_style]
			(  (p ~a:[a_style pause_title_style][]) 
			:: (p ~a:[a_style stats_style][]) 
			:: (p ~a:[a_style stats_title_style][]) 
			:: [close_button])
	in
	let overlay =
		div ~a:[a_style overlay_style] [modal_box]
	in
	let overlay_node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div overlay in
	let close_node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_button close_button in
	Dom.appendChild body overlay_node;

	(* a promise that only resolves when something explicitly triggers it *)
	let external_close, wakener = wait () in
	async (fun () ->
		pick [
			(Lwt_js_events.click close_node >|= fun _ -> ());
			external_close;
		] >>= fun () ->
		Dom.removeChild body overlay_node;
		close_modal := None;
		return_unit
	);

	(* the closer callback for external callers, e.g. a keypress handler *)
	close_modal := Some (fun () -> wakeup wakener ());
	()