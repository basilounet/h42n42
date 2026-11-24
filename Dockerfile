FROM    	ocaml/opam:ubuntu-25.04-ocaml-5.5


# Install dependencies
RUN     	[   "opam", "update"                						]
RUN     	[   "opam", "install", "--update-invariant",				\
				"-y",													\
				"dune",													\
				"tyxml",												\
				"eliom",                        						\
				"conf-libev",                   						\
				"lwt",                          						\
				"js_of_ocaml",                  						\
				"js_of_ocaml-compiler",         						\
				"js_of_ocaml-ppx",              						\
				"js_of_ocaml-lwt",              						\
				"js_of_ocaml-tyxml",            						\
				"js_of_ocaml-toplevel",         						\
				"js_of_ocaml-ppx_deriving_json",						\
				"ocaml-lsp-server",										\
				"ocamlformat"											]
RUN				opam env | sh


# Gets the source files
ADD			[	"conf/",	"app"									]
WORKDIR		"/home/opam/app"
ADD			[	"src/",		"src/"									]
ADD			[	"static/",	"static/"								]


# Ensure permissions on files
RUN			[	"sudo", "chown", "-R", "opam:opam", "."	]
RUN			[	"sudo", "chmod", "-R", "a+rwX", 	"."	]


# Compiles OCaml
RUN			[	"opam", "exec", "--", "dune", "build"					]


# Runs the website
RUN			[	"chmod", "+x", "start.sh"								]
ENTRYPOINT	[	"./start.sh"											]
