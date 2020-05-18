all: clean jsonServerTodo jsonServerComment jsonServerUser
.PHONY: all

jsonServerTodo:
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
	jsonServerTodo.native \
	http.native \
	jsonServerTodoexternals.native

jsonServerComment:
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
	jsonServerComment.native \
	http.native \
	jsonServerCommentexternals.native

jsonServerUser:
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
	jsonServerUser.native \
	http.native \
	jsonServerUserexternals.native

run:
	./jsonServerTodo.native
	./jsonServerComment.native
	./jsonServerUser.native

clean:
	ocamlbuild -clean
