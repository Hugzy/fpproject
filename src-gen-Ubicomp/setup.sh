sudo apt-get install libcurl4-gnutls-dev -y

opam install QCheck -y -q
opam install QCSTM -y -q
opam install ppx_deriving -y -q
opam install Lwt -y -q
opam install Cohttp -y -q
opam install Cohttp-lwt-unix -y -q
opam install Yojson -y -q
opam install OCurl -y -q

make

make run
