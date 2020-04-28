all: clean apiTest ubiComp
.PHONY: all

apiTest:
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
	apiTest.native \
	http.native \
	apiTestexternals.native

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
	./apiTest.native
	./ubiComp.native

clean:
	ocamlbuild -clean
