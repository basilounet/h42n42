open Vector
open Js_of_ocaml


type t		= Types.creet
type state	= Types.creet_state


val string_of_state : state												-> string
val style_string 	: t													-> string
val update			: #Js_of_ocaml.Dom.node Js_of_ocaml.Js.t	-> t	-> t
val be_contaminated : t													-> t
val be_dead			: t													-> t
val be_grabbed		: t													-> t
val be_released		: t													-> t


val create  		: int										-> int	-> t
val advance			: t													-> t
val	avoidance		: t													-> float
val	avoidance2		: t													-> float
val debug_creet		: t													-> unit