FROM    ocaml/opam:ubuntu-25.04-ocaml-5.5


RUN     [   "opam", "update"                			]
RUN     [   "opam", "install",							\
			"-y",										\
			"dune",										\
            "eliom",                        			\
            "conf-libev",                   			\
            "lwt",                          			\
            "js_of_ocaml",                  			\
            "js_of_ocaml-compiler",         			\
            "js_of_ocaml-ppx",              			\
            "js_of_ocaml-lwt",              			\
            "js_of_ocaml-tyxml",            			\
            "js_of_ocaml-toplevel",         			\
            "js_of_ocaml-ppx_deriving_json" 			]
RUN			opam env | sh


RUN     [   "mkdir", "-p", "/home/opam/app"     		]
RUN     [   "mkdir", "-p", "/home/opam/app/src"     	]
RUN     [   "mkdir", "-p", "/home/opam/app/static"  	]
RUN     [   "mkdir", "-p", "/home/opam/app/src/_build" 	]


ADD		[	"conf/run_builder.sh", "/home/opam/"		]
CMD     [   "sh", "run_builder.sh"						]