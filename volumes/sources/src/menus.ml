open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js
open Lwt
open Js_of_ocaml_tyxml.Tyxml_js.Html


let close_modal : (unit -> unit) option ref = ref None

let is_pause (): bool = match !close_modal with
	| Some _	-> true
	| None 		-> false

type slice = {
	value : float;   (* relative weight; doesn't need to sum to 100 *)
	color : string;
	icon  : string;  (* path to the icon image *)
}

let pi = 4.0 *. atan 1.0

(* point on a circle of given radius/center at angle (radians, 0 = up, clockwise) *)
let point_on_circle ~cx ~cy ~r angle =
	let x = cx +. r *. sin angle in
	let y = cy -. r *. cos angle in
	(x, y)

let make_pie_chart
	~(cx : float) ~(cy : float) ~(radius : float)
	~(icon_radius_ratio : float) (* how far from center icons sit, 0..1 *)
	~(icon_size : float)
	(slices : slice list) =
	let total = List.fold_left (fun acc s -> acc +. s.value) 0. slices in
	let _, paths, icons =
		List.fold_left
		  (fun (start_angle, paths, icons) s ->
			let sweep = 2. *. pi *. (s.value /. total) in
			let end_angle = start_angle +. sweep in
			let x0, y0 = point_on_circle ~cx ~cy ~r:radius start_angle in
			let x1, y1 = point_on_circle ~cx ~cy ~r:radius end_angle in
			let large_arc = if sweep > pi then 1 else 0 in
			let d =
				Printf.sprintf
				  "M %f %f L %f %f A %f %f 0 %d 1 %f %f Z"
				  cx cy x0 y0 radius radius large_arc x1 y1
			in
			let slice_path =
				Svg.path ~a:[Svg.a_d d; Svg.a_fill (`Color (s.color, None))] []
			in
			let mid_angle = start_angle +. sweep /. 2. in
			let icon_r = radius *. icon_radius_ratio in
			let ix, iy = point_on_circle ~cx ~cy ~r:icon_r mid_angle in
			let icon_img =
				Svg.image
				  ~a:[
					Svg.a_href s.icon;
					Svg.a_x (ix -. icon_size /. 2., None);
					Svg.a_y (iy -. icon_size /. 2., None);
					Svg.a_width (icon_size, None);
					Svg.a_height (icon_size, None);
				  ] []
			in
			(end_angle, paths @ [slice_path], icons @ [icon_img]))
		  (0., [], [])
		  slices
	in
	svg
	  ~a:[
		Svg.a_id "chart";
		Svg.a_viewBox (0., 0., cx *. 2., cy *. 2.);
		Svg.a_class ["pie_chart"];
	  ]
	  (paths @ icons)

let my_pie curr_alive contaminated evolution dead = make_pie_chart
	~cx:100. ~cy:100. ~radius:100.
	~icon_radius_ratio:0.6
	~icon_size:30.
	[
		{ value = curr_alive -. contaminated -. evolution; color = "#ec7c30"; icon = "/static/images/UI/icons/reproduction.png" };
		{ value = contaminated; color = "#9e8484"; icon = "/static/images/UI/icons/contamination.png" };
		{ value = evolution; color = "#1da546"; icon = "/static/images/UI/icons/evolution.png" };
		{ value = dead; color = "#000000"; icon = "/static/images/UI/icons/dead.png" };
	]

let pause_menu body : unit =
	let close_button =
		button ~a:[a_class ["pause_button"]; a_id "return_button"] [txt "Go back to Game"]
	in
	let slider_input (id: string) (min: int) (max: int) (value: string) =
		input () ~a:[
			a_input_type `Range; a_id id;
			a_input_min (`Number min); a_input_max (`Number max);
			a_value value; a_step (Some 1.) ] in

	let volume_input =			slider_input "volume" 			0 100 "50" in
	let reproduction_input =	slider_input "reproduction"		0 100 "50" in
	let initial_creets_input =	slider_input "initial_creets"	0 100 "50" in
	let contamination_input =	slider_input "contamination"	0 100 "50" in
	let evolution_input =		slider_input "evolution"		0 100 "50" in
	let speed_input = 			slider_input "speed" 			0 100 "50" in


	let create_slider (icon: string) (x: string) (y: string) input =
	div ~a:[a_class ["slider"]; a_style ("left:"^x^"vw;top:"^y^"vh"); a_id icon] [
		p ~a:[a_class ["slider_label"]; a_style ("background-image: url('/static/images/UI/icons/"^icon^".png')")] [];
		p ~a:[a_class ["text"]; a_style "top: -4vh; left: 7vw"]	[txt icon];
		input;
	] in

	let single_stat (icon: string) (name: string) (x: string) (y: string) (value: string) = 
	div ~a:[a_class ["single_stat"]; a_style ("left:"^x^"vw;top:"^y^"vh"); a_id name] [
		p ~a:[a_class ["single_stat"]; a_style ("left:0vw;top:0vh;background-image: url('/static/images/UI/icons/"^icon^".png')")] [];
		p ~a:[a_class ["text"]; a_style "top: 1vh; left: 5vw"; a_id (name^"_value")] [txt value];
		p ~a:[a_class ["text"]; a_style "top: 6vh; left: 5vw; font-size:0.8vw"; a_id (name^"_id")] [txt name];
	] in

	let pause_html = [
		(p ~a:[a_class ["pause_bg"]][]);
		(p ~a:[a_class ["pause_title"]][]);
		(close_button);
		(button ~a:[a_class ["pause_button"]; a_id "retry_button"; a_style "top: 60vh; left: 50%;"] [txt "Retry"]); (* TODO : here *)
		(p ~a:[a_class ["text"]; a_style "font-size: 2.2vw;"][txt "Difficulty"]);
		(p ~a:[a_class ["text"]; a_style "top: 73vh; left: 45vw"][txt "Easy"]);
		(p ~a:[a_class ["text"]; a_style "top: 73vh; left: 50vw"][txt "Normal"]);
		(p ~a:[a_class ["text"]; a_style "top: 73vh; left: 55vw"][txt "Hard"]);
		(button ~a:[a_class ["difficulty"]; a_id "box_Easy"  ; a_style "left: 45vw;"][]); (* TODO : here *)
		(button ~a:[a_class ["difficulty"]; a_id "box_Normal"; a_style "left: 50vw;"][]); (* TODO : here *)
		(button ~a:[a_class ["difficulty"]; a_id "box_Hard"  ; a_style "left: 55vw"][]);  (* TODO : here *)
	] in 
	let settings_html = [
	   (p ~a:[a_class ["settings_title"]][]);
	   (p ~a:[a_class ["settings"]][]);
	   (create_slider "volume"			"20" "31"	volume_input);
	   (create_slider "reproduction" 	"20" "42"	reproduction_input);
	   (create_slider "initial creets"	"20" "53"	initial_creets_input);
	   (create_slider "contamination"	"20" "64"	contamination_input);
	   (create_slider "evolution"		"20" "75"	evolution_input);
	   (create_slider "speed"			"20" "86"	speed_input);
	] in
	let stats_html = [
		(p ~a:[a_class ["stats_title"]][]);
		(p ~a:[a_class ["stats"]][]);
		(p ~a:[a_class ["text"]; a_style "top: 17vh; left: 83vw; font-size: 3vw"] [txt "14:50"]);  (* TODO : here *)
		(p ~a:[a_class ["text"]; a_style "top: 26vh; left: 83vw; font-size: 2.2vw; width: 20vw"] [txt "Score: 424242"]);  (* TODO : here *)
		(single_stat "reproduction" "Healed" "78" "39" "12"); (* TODO : here *)
		(single_stat "max_alive" "Max alive" "88" "39" "42"); (* TODO : here *)
		(single_stat "contamination" "Contaminated" "78" "48" "20"); (* TODO : here *)
		(single_stat "evolution" "Mean / Berserk" "88" "48" "4"); (* TODO : here *)
		(p ~a:[a_class ["divider"]] []);
		(my_pie 80. 42. 10. 16.) (* TODO : here *)
	] in
	let overlay = div ~a:[a_class ["pause_overlay"]] (pause_html @ settings_html @ stats_html) in

	let volume_node =			Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input volume_input in
	let reproduction_node =		Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input reproduction_input in
	let initial_creets_node =	Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input initial_creets_input in
	let contamination_node =	Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input contamination_input in
	let evolution_node =		Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input evolution_input in
	let speed_node =			Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input speed_input in
	let close_node =			Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_button close_button in
	let overlay_node =			Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div overlay in
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
			Printf.printf "%s: %.0f\n" name v; (* TODO : here *)
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