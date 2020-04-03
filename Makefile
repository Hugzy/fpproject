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

clean:
	ocamlbuild -clean

	