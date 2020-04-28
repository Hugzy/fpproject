all: clean ubicomp
.PHONY: all

ubicomp:
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
	ubicomp.native \
	http.native \
	ubicompexternals.native

run:
	./ubicomp.native

clean:
	ocamlbuild -clean
