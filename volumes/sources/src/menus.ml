open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt
open Js_of_ocaml_tyxml.Tyxml_js.Html


let close_modal : (unit -> unit) option ref = ref None


let is_pause (): bool = match !close_modal with
	| Some _	-> true
	| None 		-> false

let overlay_style =
	"position: absolute; top: 0; left: 0; width: 100vw; height: 100vh; \
	 background: rgba(0,0,0,0.6); display: flex; align-items: center; \
	 justify-content: center; z-index: 1000;"

let box_style =
	"background: white; padding: 2vw; border-radius: 8px; \
	 min-width: 20vw; max-width: 60vw; text-align: center;"

let button_style =
	"margin-top: 1.5vw; padding: 0.5vw 1.5vw; cursor: pointer;"

let show_modal body title content button_label : unit =
	let close_button =
		button ~a:[a_style button_style] [txt button_label]
	in
	let modal_box =
		div ~a:[a_style box_style]
			(h2 [txt title] :: content @ [close_button])
	in
	let overlay =
		div ~a:[a_style overlay_style] [modal_box]
	in
	let overlay_node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div overlay in
	let close_node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_button close_button in
	Dom.appendChild body overlay_node;

	let closed = ref false in
	let remove_from_dom () =
		if not !closed then begin
			closed := true;
			Dom.removeChild body overlay_node end in

	(* a promise that only resolves when something explicitly triggers it *)
	let external_close, wakener = wait () in
	async (fun () ->
		pick [
			(Lwt_js_events.click close_node >|= fun _ -> ());
			external_close;
		] >>= fun () ->
		remove_from_dom ();
		close_modal := None;
		return_unit
	);

	(* the closer callback for external callers, e.g. a keypress handler *)
	close_modal := Some (fun () ->if not !closed then wakeup wakener ());
	()