open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html

val sound_effect_sound: float ref
type sound_effect_type = SContamination | SEvolution | SHealing | SDeath | SReproduction

val activated_sounds: bool list ref

val sound_effect_type_to_int: sound_effect_type -> int
val toggle_sound: sound_effect_type -> unit

val bgm_node: Js_of_ocaml.Dom_html.audioElement Js_of_ocaml.Js.t


val setup_background_music: Dom_html.bodyElement Js.t -> unit
(* val start_music_on_first_interaction: Dom_html.bodyElement Js.t -> unit *)

val play_sound_effect:	sound_effect_type -> unit
val play_click:					string						-> unit
val play_menu:					string						-> unit
