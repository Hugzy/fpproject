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
	ocamlbuild -use-ocamlfind -package qcheck src/main.byte

clean:
	ocamlbuild -clean

	