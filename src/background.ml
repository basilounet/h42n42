open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_lwt
open Vector


type tags_type = {
	river: string;
	hospital: string;
	grass: string;
}


let tags: tags_type = {
	river = "RIVER";
	hospital = "HOSPITAL";
	grass = "GRASS";
}


let river =
	img
		~src:"/static/images/backgrounds/river/river_0.png"
		~alt:(tags.river)
		~a:[a_class [tags.river]; 
			a_draggable false]
		()


let hospital =
	img
		~src:"/static/images/backgrounds/hospital/hospitals.png"
		~alt:(tags.hospital)
		~a:[a_class [tags.hospital]; 
			a_draggable false]
		()


let grass =
	img
		~src:"/static/images/backgrounds/grass/grass_0.png"
		~alt:(tags.grass)
		~a:[a_class [tags.grass]; 
			a_draggable false]
		()


let rec animation (anim_name: string) (anim_curr: int) (anim_max: int) (anim_wait: float) (anim_time: float) node : unit Lwt.t =
		node##.src := Js.string @@ "/static/images/" ^ anim_name ^ "_" ^ (string_of_int anim_curr) ^ ".png";
	match anim_curr with
	| i when i >= anim_max -> 
		Lwt.bind (Lwt_js.sleep anim_wait) 
		(fun () -> animation anim_name 0 anim_max anim_wait anim_time node)
	| _ ->
		Lwt.bind (Lwt_js.sleep anim_time) 
		(fun () -> animation anim_name (anim_curr + 1) anim_max anim_wait anim_time node)


let grass_node = ref None


let get_wsize () : vec2 = match !grass_node with
	| None			-> vec2 (Int (0))						(Int (0))
	| Some node	-> vec2 (Int (node##.width)) (Int (node##.height))


let is_in_river (x_coord: float): bool =
	x_coord >= Params.simulation.river_start


let is_in_hospital (x_coord: float): bool =
	x_coord <= Params.simulation.hospital_end

type live_stat = {
	elt : Html_types.div_content elt;
	set : string -> unit;
}

let single_stat (icon: string) (name: string) (x: string) (y: string) (value: string) : live_stat =
	let value_p =
		p ~a:[a_class ["text"]; a_style "top: 35%; left: 70%"; a_id (name ^ "_value")] [txt value]
	in
	let e = div ~a:[a_class ["single_stat"]; a_style ("left:" ^ x ^ "%;top:" ^ y ^ "%"); a_id name] [
		p ~a:[a_class ["single_stat"]; a_style ("width:25%;height:50%;left:45%;top:37.5%;background-image: url('/static/images/" ^ icon ^ ".png')")] [];
		value_p;
		p ~a:[a_class ["text"]; a_style "top: 65%; left: 70%; font-size:0.8vw"; a_id (name ^ "_id")] [txt name];
	]
	in
	let value_node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_p value_p in
	{ elt = e; set = (fun s -> value_node##.textContent := Js.some (Js.string s)) }


let make_live_text ~id ~class_ ~style ~initial : live_stat =
	let e = p ~a:[a_class class_; a_id id; a_style style] [txt initial] in
	let node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_p e in
	{ elt = e; set = (fun s -> node##.textContent := Js.some (Js.string s)) }


let score			= make_live_text ~id:"rt_score" ~class_:["text"] ~style:"top: 73%; left: 50%; font-size: 1.8vw;" ~initial:"Score: 0"
let time			= make_live_text ~id:"rt_time"	~class_:["text"] ~style:"top: 67%; left: 50%; font-size: 1vw;" ~initial:"00:00"
let healed		= single_stat "UI/icons/reproduction" "Healed"	"14" "73" "0"
let dead			= single_stat "UI/icons/dead"					"Dead"		"86" "73" "0"
let healthy		= single_stat "creets/healthy"				"Healthy"	"14" "40" "0"
let sick			=	single_stat "creets/sick"						"Sick"		"38" "40" "0"
let berserk		=	single_stat "creets/berserk"				"Berserk" "62" "40" "0"
let mean			=	single_stat "creets/mean"						"Mean"		"86" "40" "0"
let fps				= make_live_text ~id:"rt_fps"	 ~class_:["text"] ~style:"top: 22%; left: 111%; font-size: 0.65vw; text-align: left;" ~initial:"FPS: 120"

let real_time_stats body : unit =
	let content =
		div ~a:[a_id "real_time"; a_class ["real_time_bg"]]
		  [score.elt; time.elt; healed.elt; dead.elt;
		   healthy.elt; sick.elt; berserk.elt; mean.elt;
		   fps.elt]
	in
	let toggle =
		input () ~a:[a_input_type `Checkbox; a_id "real_time_checkbox"; a_class ["real_time_toggle"]]
	in
	let toggle_label =
		label ~a:[a_label_for "real_time_checkbox"; a_class ["real_time_label"]] []
	in
	let wrapper = div ~a:[a_class ["real_time_wrapper"]] [toggle; toggle_label; content] in
	Dom.appendChild body (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div wrapper)

let update_stats () : unit =
	score.set		(Printf.sprintf "Score: %d" @@ Statistics.stats.score#get ());
	time.set		(Utils.time_elapsed_format ());
	healed.set	(Statistics.stats.healed#get () |> string_of_int);
	dead.set		(Statistics.stats.dead#get () |> string_of_int);
	healthy.set	(Statistics.stats.healthy#get () |> string_of_int);
	sick.set		(Statistics.stats.sick#get () |> string_of_int);
	berserk.set	(Statistics.stats.berserk#get () |> string_of_int);
	mean.set		(Statistics.stats.mean#get () |> string_of_int);
	fps.set			(Printf.sprintf "FPS: %.0f" (Float.floor (1. /. Params.simulation.delta_time#get ())));
	()

let create body : unit = 
	grass_node := Some (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img grass);
	match !grass_node with
	| None -> ()
	| Some node ->
	Dom.appendChild body node;
	Lwt.async (fun () -> animation "backgrounds/grass/grass" 0 83 25. 0.095 node);
	let river_node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img river in
	Dom.appendChild body river_node;
	Lwt.async (fun () -> animation "backgrounds/river/river" 0 45 20. 0.115 river_node);
	Dom.appendChild body @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img river;
	Dom.appendChild body @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img hospital;
		
	real_time_stats body;
	()

