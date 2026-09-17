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
		button ~a:[a_class ["pause_button"]; a_id "return_button"] [txt "Go back to Game"]
	in
	let volume_input =
		input () ~a:[
			a_input_type `Range; a_id "volume";
			a_input_min (`Number 0); a_input_max (`Number 100);
			a_value "50"; a_step (Some 1.) ] in

	let reproduction_input =
		input () ~a:[
			a_input_type `Range; a_id "reproduction";
			a_input_min (`Number 0); a_input_max (`Number 100);
			a_value "50"; a_step (Some 1.) ] in

	let initial_creets_input =
		input () ~a:[
			a_input_type `Range; a_id "initial_creets";
			a_input_min (`Number 0); a_input_max (`Number 100);
			a_value "50"; a_step (Some 1.) ] in
	
	let contamination_input =
		input () ~a:[
			a_input_type `Range; a_id "contamination";
			a_input_min (`Number 0); a_input_max (`Number 100);
			a_value "50"; a_step (Some 1.) ] in

	let evolution_input =
		input () ~a:[
			a_input_type `Range; a_id "evolution";
			a_input_min (`Number 0); a_input_max (`Number 100);
			a_value "50"; a_step (Some 1.) ] in

	let speed_input =
		input () ~a:[
			a_input_type `Range; a_id "speed";
			a_input_min (`Number 0); a_input_max (`Number 100);
			a_value "50"; a_step (Some 1.) ] in

	let create_slider (icon: string) (x: string) (y: string) input =
	div ~a:[a_class ["slider"]; a_style ("left:"^x^"vw;top:"^y^"vh"); a_id icon] [
		p ~a:[a_class ["slider_label"]; a_style ("background-image: url('/static/images/UI/icons/"^icon^".png')")] [];
		p ~a:[a_class ["text"]; a_style "top: -4vh; left: 7vw"] [txt icon];
		input;
	] in

	let pause_html = [
		(p ~a:[a_class ["pause_bg"]][]);
		(p ~a:[a_class ["pause_title"]][]);
		(close_button);
		(button ~a:[a_class ["pause_button"]; a_id "retry_button"; a_style "top: 60vh; left: 50%;"] [txt "Retry"]);
		(p ~a:[a_class ["text"]; a_style "font-size: 2.2vw;"][txt "Difficulty"]);
		(p ~a:[a_class ["text"]; a_style "top: 73vh; left: 45vw"][txt "Easy"]);
		(p ~a:[a_class ["text"]; a_style "top: 73vh; left: 50vw"][txt "Normal"]);
		(p ~a:[a_class ["text"]; a_style "top: 73vh; left: 55vw"][txt "Hard"]);
		(button ~a:[a_class ["difficulty"]; a_id "box_Easy"  ; a_style "left: 45vw;"][]);
		(button ~a:[a_class ["difficulty"]; a_id "box_Normal"; a_style "left: 50vw;"][]);
		(button ~a:[a_class ["difficulty"]; a_id "box_Hard"  ; a_style "left: 55vw"][]) ]
	in 
	let settings_html = [
	   (p ~a:[a_class ["settings_title"]][]);
	   (p ~a:[a_class ["settings"]][]);
	   (create_slider "volume"			"20" "30"	volume_input);
	   (create_slider "reproduction" 	"20" "40"	reproduction_input);
	   (create_slider "initial creets"	"20" "50"	initial_creets_input);
	   (create_slider "contamination"	"20" "60"	contamination_input);
	   (create_slider "evolution"		"20" "70"	evolution_input);
	   (create_slider "speed"			"20" "80"	speed_input);
	   ]
	in
	let stats_html = [
		(p ~a:[a_class ["stats_title"]][]);
		(p ~a:[a_class ["stats"]][]) ]
  in
	let overlay = div ~a:[a_class ["pause_overlay"]] (pause_html @ settings_html @ stats_html) in

	let volume_node =			Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input volume_input in
	let reproduction_node =		Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input reproduction_input in
	let initial_creets_node =	Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input initial_creets_input in
	let contamination_node =	Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input contamination_input in
	let evolution_node =		Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input evolution_input in
	let speed_node =			Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input speed_input in
	let overlay_node =			Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div overlay in
	let close_node =			Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_button close_button in
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

	(* Other callbacks for of the page, e.g. a slider / button *)
	let setup_slider_listener (name: string) node_to_listen = Lwt.async (fun () ->
		Lwt_js_events.inputs node_to_listen (fun _ev _handler ->
			let v = Js.to_string node_to_listen##.value |> float_of_string in
			Printf.printf "%s: %.0f\n" name v;
			Lwt.return_unit
		);
	); () in
	setup_slider_listener "volume" 			volume_node;
	setup_slider_listener "reproduction" 	reproduction_node;
	setup_slider_listener "initial_creets" 	initial_creets_node;
	setup_slider_listener "contamination" 	contamination_node;
	setup_slider_listener "evolution" 		evolution_node;
	setup_slider_listener "speed" 			speed_node;

	()