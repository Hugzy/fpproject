.PHONY: clean main

build:
	ocamlbuild -r \
	-use-ocamlfind \
	-package \
	qcheck,\
	cohttp-lwt-unix,\
	cohttp,\
	lwt \
	-tag thread \
	playground/clientexample.native

main:
	ocamlbuild -r \
	-use-ocamlfind \
	-package \
	qcheck,\
	cohttp-lwt-unix,\
	cohttp,\
	lwt,\
	yojson,\
	ppx_deriving.show,\
	qcstm,\
	curl \
	-tag thread \
	src/main.native \
	src/http.native \
	src/externals.native

run: 
	./main.native

clean:
	ocamlbuild -clean
	cd playground
	ocamlbuild -clean

