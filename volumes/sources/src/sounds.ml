open Lwt
open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js.Html


let sound_effect_sound: float ref = ref 1.
type sound_effect_type = SContamination | SHealing | SDeath | SReproduction

let activated_sounds = ref [true; true; true; true]

let sound_effect_type_to_int = function
	| SContamination -> 0
	| SHealing -> 1
	| SDeath -> 2
	| SReproduction -> 3

let sound_effect_type_to_source = function
	| SContamination -> "/static/sounds/game/infected.wav"
	| SHealing -> "/static/sounds/game/heal.wav"
	| SDeath -> "/static/sounds/game/game_lost.wav"
	| SReproduction -> "/static/sounds/game/reproduction.wav"

let toggle_sound (sound_type: sound_effect_type) =
	let pos = sound_effect_type_to_int sound_type in
	activated_sounds := List.mapi (fun i x -> if i = pos then not x else x) !activated_sounds;
	Printf.printf "sound_type: %d, value: %b\n" pos (List.nth !activated_sounds pos)

let playlist = [
	"/static/sounds/musics/main_theme.mp3";
	"/static/sounds/musics/jazz_theme.mp3";
	(* loops back after the last one *)
]

let audio_elt (source: string) =
	audio ~a:[a_id "audio"] ~src:(source) []

let audio_node (source: string) = audio_elt source |> Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_audio
let bgm_node = audio_node (List.hd playlist)

let current_track = ref 0

let play_track i =
	let track = List.nth playlist i in
	bgm_node##.src := Js.string track;
	bgm_node##play |> ignore

let setup_background_music body =
	bgm_node##.volume := Js.number_of_float 0.;
	bgm_node##.onended := Dom_html.handler (fun _ ->
		current_track := (!current_track + 1) mod List.length playlist;
		play_track !current_track;
		Js._true
	);
	Dom.appendChild body bgm_node;
	play_track 0

let play_sound_effect (sound_type: sound_effect_type) : unit = 
	match (sound_effect_type_to_int sound_type) |> List.nth !activated_sounds with
	| false -> ()
	| true ->
	let audio_node = audio_node @@ sound_effect_type_to_source sound_type in
	audio_node##.volume := Js.number_of_float !sound_effect_sound;
	audio_node##play |> ignore
