open Lwt
open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js
open Js_of_ocaml_tyxml.Tyxml_js.Html


let close_modal : (unit -> unit) option ref = ref None

let is_pause (): bool = match !close_modal with
	| Some _	-> true
	| None 		-> false

let float_of_js_string str: float = str |> Js.to_string |> float_of_string
let js_string_of_float (flt: float) = Printf.sprintf "%.0f" flt |> Js.string
let number_of_js_string v = v |> float_of_js_string |> Js.number_of_float


type slice = {
	value : float;	 (* relative weight; doesn't need to sum to 100 *)
	color : string;
	icon	: string; (* path to the icon image *)
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
	match total with
	| 0. -> svg ~a:[ Svg.a_id "chart";Svg.a_viewBox (0., 0., 0., 0.);Svg.a_class ["pie_chart"]]([])
	| total ->
	let _, paths, icons = List.fold_left
		(fun (start_angle, paths, icons) s ->
		match s.value with
		| 0. -> (start_angle, paths, icons)
		| _ ->
		let sweep = 2. *. pi *. (s.value /. total) in
		let end_angle = start_angle +. sweep -. 0.0000001 in (* avoid 2pi *)
		let x0, y0 = point_on_circle ~cx ~cy ~r:radius start_angle in
		let x1, y1 = point_on_circle ~cx ~cy ~r:radius end_angle in
		let large_arc = if sweep > pi then 1 else 0 in
		let d = Printf.sprintf
			"M %f %f L %f %f A %f %f 0 %d 1 %f %f Z"
			cx cy x0 y0 radius radius large_arc x1 y1
		in
		let slice_path =
			Svg.path ~a:[Svg.a_d d; Svg.a_fill (`Color (s.color, None))] []
		in
		let mid_angle = start_angle +. sweep /. 2. in
		let icon_r = radius *. icon_radius_ratio in
		let ix, iy = point_on_circle ~cx ~cy ~r:icon_r mid_angle in
		let icon_img = Svg.image
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
	~icon_size:40.
	[
		{ value = curr_alive; color = "#ec7c30"; icon = "/static/images/UI/icons/reproduction.png" };
		{ value = contaminated; color = "#9e8484"; icon = "/static/images/UI/icons/contamination.png" };
		{ value = evolution; color = "#1da546"; icon = "/static/images/UI/icons/evolution.png" };
		{ value = dead; color = "#000000"; icon = "/static/images/UI/icons/dead.png" };
	]

type live_stat = {
	elt : Html_types.div_content elt;
	set : string -> unit;
}
let close_button = button ~a:[a_class ["pause_button"]; a_id "return_button"] [txt "Go back to Game"]
let retry_button = button ~a:[a_class ["pause_button"]; a_id "retry_button"; a_style "top: 60vh; left: 50%;"] [txt "Retry"]
let easy_button = button ~a:[a_class ["difficulty"]; a_id "box_easy"	; a_style "left: 45vw;"][]
let normal_button = button ~a:[a_class ["difficulty"]; a_id "box_normal"; a_style "left: 50vw;"][]
let hard_button = button ~a:[a_class ["difficulty"]; a_id "box_hard"	; a_style "left: 55vw"][]
let slider_input (id: string) (min: int) (max: int) (step: float) (value: string) =
	input () ~a:[
		a_input_type `Range; a_id id;
		a_input_min (`Number min); a_input_max (`Number max);
		a_value value; a_step (Some step) ]
	
let sound_effects_input =		slider_input "sound_effects" 	0 1	 0.05	"50"
let music_input =						slider_input "music"					0 1 	0.05	"0"	
let reproduction_input =		slider_input "reproduction"		0 10	1.		"50"
let initial_creets_input =	slider_input "initial_creets"	0 10	1.		"50"
let contamination_input =		slider_input "contamination"	0 10	1.		"50"
let evolution_input =				slider_input "evolution"			0 10	1.		"50"
let speed_input = 					slider_input "speed" 					0 10	1.		"50"

let checkbox_input (id : string) (checked : bool) =
	let base_attrs = [a_input_type `Checkbox; a_id id; a_class ["custom_checkbox"]] in
	input () ~a:(if checked then a_checked () :: base_attrs else base_attrs)

let sound_contamination_input =	checkbox_input "sound_contamination" true
let sound_evolution_input =			checkbox_input "sound_evolution" true
let sound_heal_input = 					checkbox_input "sound_heal" true
let sound_dead_input = 					checkbox_input "sound_dead" true
let sound_reproduction_input =	checkbox_input "sound_reproduction" true

let create_slider (icon: string) (x: string) (y: string) slider =
div ~a:[a_class ["slider"]; a_style ("left:"^x^"vw;top:"^y^"vh"); a_id icon] [
	p ~a:[a_class ["slider_icon"]; a_style ("background-image: url('/static/images/UI/icons/"^icon^".png')")] [];
	p ~a:[a_class ["text"]; a_style "top: 0; left: 50%"]	[txt icon];
	slider;
]

let create_checkbox (icon: string) (x: string) (y: string) checkbox = 
div ~a:[a_style ("position:absolute;left:"^x^"vw;top:"^y^"vh;width:3vw;height:3vw"); a_id ("sound_"^icon)] [
	p ~a:[a_class ["check_label"]; a_style ("background-image: url('/static/images/UI/icons/"^icon^".png')")] [];
	checkbox;		
]

let single_stat (icon: string) (name: string) (x: string) (y: string) (value: string) = 
div ~a:[a_class ["single_stat"]; a_style ("left:"^x^"%;top:"^y^"%"); a_id name] [
	p ~a:[a_class ["single_stat"]; a_style ("width:5vw;left:25%;top:37.5%;background-image: url('/static/images/UI/icons/"^icon^".png')")] [];
	p ~a:[a_class ["text"]; a_style "top: 30%; left: 70%"; a_id (name^"_value")] [txt value];
	p ~a:[a_class ["text"]; a_style "top: 70%; left: 70%; font-size:0.8vw"; a_id (name^"_id")] [txt name];
]

let sound_effects_node =			Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input sound_effects_input
let music_node =							Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input music_input
let sound_contamination_node =Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input sound_contamination_input
let sound_evolution_node =		Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input sound_evolution_input
let sound_heal_node =					Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input sound_heal_input
let sound_dead_node =					Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input sound_dead_input
let sound_reproduction_node =	Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input sound_reproduction_input
let reproduction_node =				Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input reproduction_input
let initial_creets_node =			Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input initial_creets_input
let contamination_node =			Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input contamination_input
let evolution_node =					Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input evolution_input
let speed_node =							Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input speed_input
let close_node =							Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_button close_button
let retry_node =							Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_button retry_button
let easy_node =								Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_button easy_button
let normal_node =							Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_button normal_button
let hard_node =								Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_button hard_button


let set_slider_value slider value fun_of_value	= 
	slider##.value := value;
	fun_of_value value

let music_fun v = 
	Sounds.bgm_node##.volume := number_of_js_string v

let unused_fun v = (* TODO : remove when everything is implemented *)
	(* Sounds.bgm_node##.volume := number_of_js_string v *)
	()

let sound_effect_fun v = 
	(* Printf.printf "v: %s, sound_effect_sound: %f\n" (Js.to_string v) !Sounds.sound_effect_sound; *)
	Sounds.sound_effect_sound := float_of_js_string v

(* Other callbacks for of the page, e.g. a slider / button *)
let setup_slider_listener (name: string) node_to_listen fun_of_value = 
	async (fun () -> Lwt_js_events.inputs node_to_listen (fun _ev _handler ->
		(* let v = Js.to_string node_to_listen##.value |> float_of_string in
		Printf.printf "%s: %.0f\n" name v; *)
		set_slider_value node_to_listen node_to_listen##.value fun_of_value;
		Sounds.play_click "3";

		Lwt.return_unit
	)); ()

let setup_checkbox_listener (name : string) (sound_type: Sounds.sound_effect_type) node on_toggle =
	Lwt.async (fun () ->
	Lwt_js_events.clicks node (fun _ev _handler ->
		(* let is_checked = Js.to_bool node##.checked in *)
		(* Printf.printf "%s: %b\n" name is_checked; *)
		Sounds.play_click @@ string_of_int (Random.int 4);
		on_toggle sound_type ;
		Lwt.return_unit
	)); ()


let sound_check_fun (sound_type: Sounds.sound_effect_type) = 
	Sounds.toggle_sound sound_type

let setup_difficulty_buttons node_to_listen fn = async (fun () ->
	Lwt_js_events.clicks node_to_listen (fun ev _handler ->
	Sounds.play_click @@ string_of_int (Random.int 4);
	let new_val node = fn (float_of_js_string node##.min) (float_of_js_string node##.max) |> js_string_of_float in
	(* Printf.printf "new_val: %s\n" @@ Js.to_string @@ new_val reproduction_node; *)
	set_slider_value reproduction_node	 (new_val reproduction_node)		unused_fun;
	set_slider_value initial_creets_node (new_val initial_creets_node)	unused_fun;
	set_slider_value contamination_node	 (new_val contamination_node)	 unused_fun;
	set_slider_value evolution_node			 (new_val evolution_node)				unused_fun;
	set_slider_value speed_node					 (new_val speed_node)						unused_fun;
	Lwt.return_unit
)); ()

let already_setup_listeners = ref false

let one_time_setup_listeners () : unit =
	match !already_setup_listeners with
	| true -> ()
	| false ->
	already_setup_listeners := true;
	async (fun () ->
		Lwt_js_events.clicks retry_node (fun ev _handler ->
		Printf.printf "retry\n"; (* TODO : here *)
		Sounds.play_click @@ string_of_int (Random.int 4);
		(* sound_effects_node##.value := js_string_of_float @@ mid_value (float_of_js_string sound_effects_node##.min) (float_of_js_string sound_effects_node##.max); *)
		Lwt.return_unit
	));
	setup_slider_listener "sound_effects" 	sound_effects_node	sound_effect_fun;
	setup_slider_listener "music" 					music_node					music_fun;
	setup_slider_listener "reproduction" 		reproduction_node		unused_fun;
	setup_slider_listener "initial_creets" 	initial_creets_node unused_fun;
	setup_slider_listener "contamination" 	contamination_node	unused_fun;
	setup_slider_listener "evolution" 			evolution_node			unused_fun;
	setup_slider_listener "speed" 					speed_node					unused_fun;

	setup_checkbox_listener "sound_contamination" Sounds.SContamination	sound_contamination_node	sound_check_fun;
	setup_checkbox_listener "sound_evolution"			Sounds.SEvolution			sound_evolution_node			sound_check_fun;
	setup_checkbox_listener "sound_heal"					Sounds.SHealing				sound_heal_node						sound_check_fun;
	setup_checkbox_listener "sound_dead"					Sounds.SDeath					sound_dead_node						sound_check_fun;
	setup_checkbox_listener "sound_reproduction"	Sounds.SReproduction	sound_reproduction_node		sound_check_fun;

	setup_difficulty_buttons easy_node		(fun min max -> min);
	setup_difficulty_buttons normal_node	(fun min max -> (max -. min) /. 2. +. min);
	setup_difficulty_buttons hard_node		(fun min max -> max);
	()

let pause_menu body : unit =
	one_time_setup_listeners ();

	let pause_html = [
		(p ~a:[a_class ["pause_bg"]][]);
		(p ~a:[a_class ["pause_title"]][]);
		(close_button);
		(retry_button);
		(p ~a:[a_class ["text"]; a_style "font-size: 2.2vw;"][txt "Difficulty"]);
		(p ~a:[a_class ["text"]; a_style "top: 77vh; left: 45vw"][txt "Easy"]);
		(p ~a:[a_class ["text"]; a_style "top: 77vh; left: 50vw"][txt "Normal"]);
		(p ~a:[a_class ["text"]; a_style "top: 77vh; left: 55vw"][txt "Hard"]);
		(easy_button);
		(normal_button);
		(hard_button); 
	] in 
	let settings_html = [
		 (p ~a:[a_class ["settings_title"]][]);
		 (p ~a:[a_class ["settings"]][]);
		 (create_slider		"music"						"20" "28"		music_input);
		 (create_slider		"sound effects"		"20" "35.5"	sound_effects_input);
		 (create_checkbox "contamination"		"8" "40"		sound_contamination_input);
		 (create_checkbox "evolution"				"12" "40"		sound_evolution_input);
		 (create_checkbox "reproduction"		"16" "40"		sound_heal_input);
		 (create_checkbox "dead"						"20" "40"		sound_dead_input);
		 (create_checkbox "max alive"				"24" "40"		sound_reproduction_input);
		 (create_slider 	"reproduction" 		"20" "53"		reproduction_input);
		 (create_slider 	"initial creets"	"20" "61"		initial_creets_input);
		 (create_slider 	"contamination"		"20" "70"		contamination_input);
		 (create_slider 	"evolution"				"20" "78"		evolution_input);
		 (create_slider 	"speed"						"20" "86"		speed_input);
	] in
	let stats_html = [
		(p ~a:[a_class ["stats_title"]][]);
		(p ~a:[a_class ["stats"]][]);
		(p ~a:[a_class ["text"]; a_style "top: 22.5vh; left: 83vw; font-size: 3vw"] [txt @@ Utils.time_elapsed_format ()]);
		(p ~a:[a_class ["text"]; a_style "top: 29vh; left: 83vw; font-size: 2.2vw; width: 20vw"] [txt @@ Printf.sprintf "Score: %d" @@ Statistics.stats.score#get ()]);
		(single_stat "reproduction" "Healed" "78" "39" @@ string_of_int @@ Statistics.stats.healed#get ());
		(single_stat "max alive" "Max alive" "88" "39" @@ string_of_int @@ Statistics.stats.max_alive#get ());
		(single_stat "contamination" "Contaminated" "78" "48" @@ string_of_int @@ Statistics.stats.contaminations#get ());
		(single_stat "evolution" "Evolutions" "88" "48" @@ string_of_int @@ Statistics.stats.evolutions#get ());
		(p ~a:[a_class ["divider"]] []);
		(my_pie 
    (float_of_int @@ Statistics.stats.healthy#get ())
    (float_of_int @@ Statistics.stats.sick#get ())
    (float_of_int @@ (Statistics.stats.mean#get () + Statistics.stats.berserk#get ()))
    (float_of_int @@ Statistics.stats.dead#get ()))
    (* curr_alive contaminated evolution dead *)
	] in
	let overlay = div ~a:[a_class ["pause_overlay"]] (pause_html @ settings_html @ stats_html) in

	let overlay_node =				Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div overlay in
	Dom.appendChild body overlay_node;

	(* a promise that only resolves when something explicitly triggers it *)
	let external_close, wakener = wait () in
	async (fun () ->
		pick [
			(Lwt_js_events.click close_node >|= fun _ -> ());
			external_close;
		] >>= fun () ->
		Sounds.play_menu "close";
		Dom.removeChild body overlay_node;
		close_modal := None;
		return_unit
	);
	(* the closer callback for external callers, e.g. a keypress handler *)
	close_modal := Some (fun () -> wakeup wakener ());

	()