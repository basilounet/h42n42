open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt
open Js_of_ocaml_tyxml.Tyxml_js.Html


let close_modal : (unit -> unit) option ref = ref None

let is_pause (): bool = match !close_modal with
	| Some _	-> true
	| None 		-> false


let pause_menu body : unit =
	let close_button =
		button ~a:[a_class ["pause_button"]] [txt "Go back to Game"]
	in
	let modal_box =
		div ~a:[a_class ["pause_bg"]]
			(  (p ~a:[a_class ["pause_title"]][]) 
			:: (p ~a:[a_class ["stats"]][]) 
			:: (p ~a:[a_class ["stats_title"]][]) 
			:: (p ~a:[a_class ["settings"]][]) 
			:: (p ~a:[a_class ["settings_title"]][]) 
			:: [close_button])
	in
	let overlay =
		div ~a:[a_class ["pause_overlay"]] [modal_box]
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