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
let int_of_js_string str: int = str |> Js.to_string |> int_of_string
let js_string_of_float (flt: float) = Printf.sprintf "%.0f" flt |> Js.string
let number_of_js_string v = v |> float_of_js_string |> Js.number_of_float


type slice = {
	value : float;	 (* relative weight; doesn't need to sum to 100 *)
	color : string;
	icon	: string; (* path to the icon image *)
	title	: string; (* hover effect *)
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
		let end_angle = start_angle +. sweep -. 0.0000001 in (* avoid exactly 2pi *)
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
				Svg.a_href		s.icon;
				Svg.a_x			 (ix -. icon_size /. 2., None);
				Svg.a_y			 (iy -. icon_size /. 2., None);
				Svg.a_width	 (icon_size, None);
				Svg.a_height	(icon_size, None);
			]
			[Svg.title (Svg.txt s.title)]
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


let pie_pause () = make_pie_chart
	~cx:100. ~cy:100. ~radius:100.
	~icon_radius_ratio:0.6
	~icon_size:40.
	[
		{ value = float_of_int @@ Statistics.stats.healthy#get (); color = "#ec7c30"; icon = "/static/images/UI/icons/reproduction.png"; title = "Healhty creets" };
		{ value = float_of_int @@ Statistics.stats.sick#get (); color = "#9e8484"; icon = "/static/images/UI/icons/contamination.png"; title = "Sick creets" };
		{ value = float_of_int @@ Statistics.stats.mean#get () + Statistics.stats.berserk#get (); color = "#1da546"; icon = "/static/images/UI/icons/evolution.png"; title = "Mean / Berserk creets" };
		{ value = float_of_int @@ Statistics.stats.dead#get (); color = "#000000"; icon = "/static/images/UI/icons/dead.png"; title = "Dead creets" };
	]

let pie_lost () = make_pie_chart
	~cx:100. ~cy:100. ~radius:100.
	~icon_radius_ratio:0.6
	~icon_size:40.
	[
		{ value = float_of_int @@ Statistics.stats.healed#get (); color = "#ec7c30"; icon = "/static/images/UI/icons/reproduction.png"; title = "Total healed creets" };
		{ value = float_of_int @@ Statistics.stats.contaminations#get (); color = "#9e8484"; icon = "/static/images/UI/icons/contamination.png"; title = "Total contaminated creets" };
		{ value = float_of_int @@ Statistics.stats.evolutions#get () + Statistics.stats.berserk#get (); color = "#1da546"; icon = "/static/images/UI/icons/evolution.png"; title = "Mean / Berserk evolutions" };
		{ value = float_of_int @@ Statistics.stats.dead#get (); color = "#000000"; icon = "/static/images/UI/icons/dead.png"; title = "Dead creets" };
	]


let close_button = button ~a:[a_class ["pause_button"]; a_id "return_button"] [txt "Go back to Game"]
let retry_button = button ~a:[a_class ["pause_button"]; a_id "retry_button"; a_style "top: 60%; left: 50%;"] [txt "Retry"]
let easy_button = button ~a:[a_class ["difficulty"]; a_id "box_easy"; a_style "left: 35%;"][]
let normal_button = button ~a:[a_class ["difficulty"]; a_id "box_normal"; a_style "left: 50%;"][]
let hard_button = button ~a:[a_class ["difficulty"]; a_id "box_hard"	; a_style "left: 65%"][]
let slider_input (id: string) (min: int) (max: int) (step: float) (value: string) =
	input () ~a:[
		a_input_type `Range; a_id id;
		a_input_min (`Number min); a_input_max (`Number max);
		a_value value; a_step (Some step) ]
	
let music_input =					slider_input "music"					0 1		0.05	"0"	
let sound_effects_input =	slider_input "sound_effects" 	0 1		0.05	"50"
let reproduction_input =	slider_input "reproduction"		5 15	1.		@@ string_of_float @@ Params.simulation.birth_interval#get ()
let initial_pop_input =		slider_input "initial_pop"		5 100	1.		@@ string_of_int @@ Params.simulation.initial_pop#get ()
let contamination_input =	slider_input "contamination"	1 4		1.		@@ string_of_int @@ Params.creet.infection#get ()
let evolution_input =			slider_input "evolution"			1 15	1.		@@ string_of_float @@ Params.creet.mutation_timer#get ()
let speed_input = 				slider_input "speed" 					250 750	1.	@@ string_of_float @@ Params.creet.initial_speed#get ()

let checkbox_input (id : string) (checked : bool) =
	let base_attrs = [a_input_type `Checkbox; a_id id; a_class ["custom_checkbox"]] in
	input () ~a:(if checked then a_checked () :: base_attrs else base_attrs)

let optimised_input =					 	checkbox_input "optimised" true
let sound_contamination_input =	checkbox_input "sound_contamination" true
let sound_evolution_input =			checkbox_input "sound_evolution" true
let sound_heal_input = 					checkbox_input "sound_heal" true
let sound_dead_input = 					checkbox_input "sound_dead" true
let sound_reproduction_input =	checkbox_input "sound_reproduction" true

let create_slider (icon: string) (x: string) (y: string) slider =
div ~a:[a_class ["slider"]; a_style ("left:"^x^"%;top:"^y^"%"); a_id icon] [
	p ~a:[a_class ["slider_icon"]; a_style ("background-image: url('/static/images/UI/icons/"^icon^".png')")] [];
	p ~a:[a_class ["text"]; a_style "top: 0; left: 50%"]	[txt icon];
	slider;
]

let create_checkbox (icon: string) (x: string) (y: string) (hover: string) checkbox = 
div ~a:[a_title hover; a_style ("position:absolute;left:"^x^"%;top:"^y^"%;width:3vw;height:3vw"); a_id ("sound_"^icon)] [
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
let optimised_node =					Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input optimised_input
let sound_contamination_node =Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input sound_contamination_input
let sound_evolution_node =		Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input sound_evolution_input
let sound_heal_node =					Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input sound_heal_input
let sound_dead_node =					Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input sound_dead_input
let sound_reproduction_node =	Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input sound_reproduction_input
let reproduction_node =				Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input reproduction_input
let initial_pop_node =				Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input initial_pop_input
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

let reproduction_fun v =
	Params.simulation.birth_interval#set (float_of_js_string v)

let initial_pop_fun v =
	Params.simulation.initial_pop#set (int_of_js_string v)

let contamination_fun v =
	Params.creet.infection#set (int_of_js_string v)

let evolution_fun v =
	let value: float = (float_of_js_string evolution_node##.max)
		-. (float_of_js_string v)
		+. (float_of_js_string evolution_node##.min) in
		(* Printf.printf "evolution: %f\n" value; *)
	Params.creet.mutation_timer#set value

let speed_fun v =
	Params.creet.initial_speed#set (float_of_js_string v)

let sound_effect_fun v = 
	Sounds.sound_effect_sound := float_of_js_string v


(* Other callbacks for of the page, e.g. a slider / button / checkbox *)
let setup_slider_listener (name: string) node_to_listen fun_of_value = 
	async (fun () -> Lwt_js_events.inputs node_to_listen (fun _ev _handler ->
		set_slider_value node_to_listen node_to_listen##.value fun_of_value;
		Sounds.play_click "3";

		Lwt.return_unit
	)); ()

let setup_checkbox_listener (name : string) (sound_type: Sounds.sound_effect_type) node on_toggle =
	Lwt.async (fun () ->
	Lwt_js_events.clicks node (fun _ev _handler ->
		Sounds.play_click @@ string_of_int (Random.int 4);
		on_toggle sound_type ;
		Lwt.return_unit
	)); ()


let sound_check_fun (sound_type: Sounds.sound_effect_type) = 
	Sounds.toggle_sound sound_type

let toggle_optimised (sound_type: Sounds.sound_effect_type) =
	Params.simulation.is_optimised#set (not @@ Params.simulation.is_optimised#get ())

let setup_difficulty_buttons node_to_listen fn = async (fun () ->
	Lwt_js_events.clicks node_to_listen (fun ev _handler ->
	Sounds.play_click @@ string_of_int (Random.int 4);
	let new_val node = fn (float_of_js_string node##.min) (float_of_js_string node##.max) |> js_string_of_float in
	(* Printf.printf "new_val: %s\n" @@ Js.to_string @@ new_val reproduction_node; *)
	set_slider_value reproduction_node	(new_val reproduction_node)		reproduction_fun;
	set_slider_value initial_pop_node 	(new_val initial_pop_node)		initial_pop_fun;
	set_slider_value contamination_node	(new_val contamination_node)	contamination_fun;
	set_slider_value evolution_node			(new_val evolution_node)			evolution_fun;
	set_slider_value speed_node					(new_val speed_node)					speed_fun;
	Lwt.return_unit
)); ()

let already_setup_listeners = ref false
let on_retry : (unit -> unit) ref = ref (fun () -> ())
let on_start : (unit -> unit) ref = ref (fun () -> ())

let one_time_setup_listeners () : unit =
	match !already_setup_listeners with
	| true -> ()
	| false ->
	already_setup_listeners := true;
	async (fun () -> (** Retry **)
		Lwt_js_events.clicks retry_node (fun ev _handler ->
		!on_retry ();
		Sounds.play_click @@ string_of_int (Random.int 4);
		Lwt.return_unit
	));
	setup_slider_listener "sound_effects" 	sound_effects_node	sound_effect_fun;
	setup_slider_listener "music" 					music_node					music_fun;
	setup_slider_listener "reproduction" 		reproduction_node		reproduction_fun;
	setup_slider_listener "initial_pop" 		initial_pop_node		initial_pop_fun;
	setup_slider_listener "contamination" 	contamination_node	contamination_fun;
	setup_slider_listener "evolution" 			evolution_node			evolution_fun;
	setup_slider_listener "speed" 					speed_node					speed_fun;

	setup_checkbox_listener "is_optimised"				Sounds.SDeath					optimised_node						toggle_optimised;
	setup_checkbox_listener "sound_contamination" Sounds.SContamination	sound_contamination_node	sound_check_fun;
	setup_checkbox_listener "sound_evolution"			Sounds.SEvolution			sound_evolution_node			sound_check_fun;
	setup_checkbox_listener "sound_heal"					Sounds.SHealing				sound_heal_node						sound_check_fun;
	setup_checkbox_listener "sound_dead"					Sounds.SDeath					sound_dead_node						sound_check_fun;
	setup_checkbox_listener "sound_reproduction"	Sounds.SReproduction	sound_reproduction_node		sound_check_fun;

	setup_difficulty_buttons easy_node		(fun min max -> min);
	setup_difficulty_buttons normal_node	(fun min max -> (max -. min) /. 2. +. min);
	setup_difficulty_buttons hard_node		(fun min max -> max);
	()

let pause_html () = div ~a:[a_class ["pause_div"]; a_id "Pause"] ([
		(p ~a:[a_class ["pause_bg"]][]);
		(p ~a:[a_class ["pause_title"]][]);
		(close_button);
		(retry_button);
		(p ~a:[a_class ["text"]; a_style "font-size: 2.2vw;"][txt "Difficulty"]);
		(p ~a:[a_class ["text"]; a_style "top: 77%; left: 35%"][txt "Easy"]);
		(p ~a:[a_class ["text"]; a_style "top: 77%; left: 50%"][txt "Normal"]);
		(p ~a:[a_class ["text"]; a_style "top: 77%; left: 65%"][txt "Hard"]);
		(easy_button);
		(normal_button);
		(hard_button); 
	])

let settings_html () = div ~a:[a_class ["settings_div"]; a_id "Settings"] ([
	(p ~a:[a_class ["settings_title"]][]);
	(p ~a:[a_class ["settings"]][]);
	(create_slider		"music"							"60"	"28"		music_input);
	(create_slider		"sound effects"			"60"	"35.5"	sound_effects_input);
	(create_checkbox "optimised"					"16"	"25"		"Toggle optimsed version"	optimised_input);
	(create_checkbox "contamination"			"30"	"40"		"Contamination sound" 		sound_contamination_input);
	(create_checkbox "evolution"					"40"	"40"		"Evolution sound" 				sound_evolution_input);
	(create_checkbox "reproduction"				"50"	"40"		"Healing sound" 					sound_heal_input);
	(create_checkbox "dead"								"60"	"40"		"Death sound" 						sound_dead_input);
	(create_checkbox "max alive"					"70"	"40"		"New creet sound" 				sound_reproduction_input);
	(create_slider 	"reproduction" 				"60"	"53"		reproduction_input);
	(create_slider 	"initial population"	"60"	"61"		initial_pop_input);
	(create_slider 	"contamination"				"60"	"70"		contamination_input);
	(create_slider 	"evolution"						"60"	"78"		evolution_input);
	(create_slider 	"speed"								"60"	"86"		speed_input);
])

let stats_html (additionnal_style: string) pie = 
	div ~a:[a_class ["stats_div"]; a_id "Stats"; a_style additionnal_style] ([
	(p ~a:[a_class ["stats_title"]][]);
	(p ~a:[a_class ["stats"]][]);
	(p ~a:[a_class ["text"]; a_style "top: 22.5%; left: 50%; font-size: 3vw"] [txt @@ Utils.time_elapsed_format ()]);
	(p ~a:[a_class ["text"]; a_style "top: 29%; left: 50%; font-size: 2.2vw; width: 75%"] [txt @@ Printf.sprintf "Score: %d" @@ Statistics.stats.score#get ()]);
	(single_stat "reproduction" "Healed" "35" "39" @@ string_of_int @@ Statistics.stats.healed#get ());
	(single_stat "max alive" "Max alive" "65" "39" @@ string_of_int @@ Statistics.stats.max_alive#get ());
	(single_stat "contamination" "Contaminations" "35" "48" @@ string_of_int @@ Statistics.stats.contaminations#get ());
	(single_stat "evolution" "Evolutions" "65" "48" @@ string_of_int @@ Statistics.stats.evolutions#get ());
	(p ~a:[a_class ["divider"]] []);
	(pie)
])


let lost_menu_node = ref None

let pause_menu () : unit =
	match !close_modal with
	| Some _ -> ()
	| None ->	
	Sounds.play_menu "open";
	let body = Dom_html.document##.body in
	one_time_setup_listeners ();

	let overlay = div ~a:[a_class ["pause_overlay"]; a_id "Pause Menu"] ([]) in

	let overlay_node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div overlay in
	Dom.appendChild body overlay_node;
	Dom.appendChild overlay_node @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div @@ pause_html ();
	Dom.appendChild overlay_node @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div @@ settings_html ();
	Dom.appendChild overlay_node @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div @@ stats_html "" @@ pie_pause ();

	(* a promise that only resolves when something explicitly triggers it *)
	let external_close, wakener = wait () in
	let do_close () =
		Sounds.play_menu "close";
		Dom.removeChild body overlay_node;
		close_modal := None;
		!on_start ()
		in
		async (fun () ->
			pick [
				(Lwt_js_events.click close_node >|= fun _ -> ());
				external_close;
			] >>= fun () -> do_close (); return_unit
				);
	(* the closer callback for external callers, e.g. a keypress handler *)
	close_modal := Some (fun () -> wakeup wakener ());
	()


let lost_menu_node = ref None

let close_lost_menu () : unit =
	match !lost_menu_node with
	| None -> ()
	| Some node ->
		Sounds.play_menu "close";
		Dom.removeChild (Dom_html.document##.body) node;
		lost_menu_node := None

let lost_menu () : unit =
	match !lost_menu_node with
	| Some _ -> ()
	| None ->
	(* Printf.printf "open lost menu"; *)
	let body = Dom_html.document##.body in
	let overlay = div ~a:[a_class ["pause_overlay"]; a_id "Lost Menu"; a_style "background: rgba(0,0,0,0.95)"] ([]) in
	let side_image src style = img
		~src:("/static/images/"^src^".png")
		~alt:(src)
		~a:[a_id src; a_draggable false; a_style (
			"position: absolute; top: 50%; left: 25%; width: 30%; height: 80%; transform: translate(-50%, -50%);"
			^ style)]
		() in

	lost_menu_node := Some (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div overlay);
	match !lost_menu_node with
	| None -> ()
	| Some node -> 
	Dom.appendChild body node;
	Dom.appendChild node @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div @@
		stats_html "top: 60%; left: 50%; width: 28%; height: 85%" @@ pie_lost ();
	Dom.appendChild node @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img @@
		side_image "UI/banners/game_lost" "top: 12%; left: 50%; width: 80%; height: 17.5%";
	Dom.appendChild node @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img @@
		side_image "creets/mean" "left: 17.5%";
	Dom.appendChild node @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img @@
		side_image "creets/berserk" "left: 82.5%";
	()
