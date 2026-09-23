open Lwt
open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js.Html


let playlist = [
  "/static/sounds/musics/main_theme.mp3";
  "/static/sounds/musics/jazz_theme.mp3";
  (* loops back after the last one *)
]

let audio_elt =
  audio ~a:[a_id "bgm"] ~src:(List.hd playlist) []

let bgm_node = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_audio audio_elt

let current_track = ref 0

let play_track i =
  let track = List.nth playlist i in
  bgm_node##.src := Js.string track;
  ignore (bgm_node##play)

let setup_background_music body =
  bgm_node##.volume := Js.number_of_float 0.;
  bgm_node##.onended := Dom_html.handler (fun _ ->
    current_track := (!current_track + 1) mod List.length playlist;
    play_track !current_track;
    Js._true
  );
  Dom.appendChild body bgm_node;
  play_track 0



let float_of_js_string str: float = str |> Js.to_string |> float_of_string
let js_string_of_float (flt: float) = Printf.sprintf "%.0f" flt |> Js.string

