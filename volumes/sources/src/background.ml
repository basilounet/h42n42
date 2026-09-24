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
	| None		-> vec2 (Int (0))            (Int (0))
	| Some node	-> vec2 (Int (node##.width)) (Int (node##.height))


let is_in_river (x_coord: float): bool =
	x_coord >= Params.simulation.river_start


let is_in_hospital (x_coord: float): bool =
	x_coord <= Params.simulation.hospital_end


let create body : vec2 = 
	grass_node := Some (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img grass);
	match !grass_node with
	| None -> get_wsize ()
	| Some node ->
	Dom.appendChild body node;
	Lwt.async (fun () -> animation "backgrounds/grass/grass" 0 83 25. 0.095 node);
	let river_node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img river in
	Dom.appendChild body river_node;
	Lwt.async (fun () -> animation "backgrounds/river/river" 0 45 20. 0.115 river_node);
	Dom.appendChild body @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img river;
	Dom.appendChild body @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_img hospital;
	get_wsize ()

