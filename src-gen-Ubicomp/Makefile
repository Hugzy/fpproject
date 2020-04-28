all: clean ubiComp
.PHONY: all

ubiComp:
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
	ubiComp.native \
	http.native \
	ubiCompexternals.native

run:
	./ubiComp.native

clean:
	ocamlbuild -clean
