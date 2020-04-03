build:
	ocamlbuild -r \
	-use-ocamlfind \
	-package \
	qcheck,\
	cohttp-lwt-unix,\
	cohttp,\
	lwt \
	-tag thread \
	playground/clienttut.native

clean:
	ocamlbuild -clean

	