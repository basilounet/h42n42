
type tags_type = {
	river: string;
	hospital: string;
	grass: string;
}

val tags: tags_type

val background_elements: ([> Html_types.div ] Js_of_ocaml_tyxml.Tyxml_js.Html.elt) list