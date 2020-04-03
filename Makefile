build:
	ocamlbuild -use-ocamlfind -package qcheck src/main.byte

clean:
	ocamlbuild -clean

