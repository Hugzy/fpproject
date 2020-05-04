all: clean jsonServer
.PHONY: all

jsonServer:
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
	jsonServer.native \
	http.native \
	jsonServerexternals.native

run:
	./jsonServer.native

clean:
	ocamlbuild -clean
