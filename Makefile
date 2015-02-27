all:
	ocamlbuild -use-menhir -libs unix -cflags -annot main.native

clean:
	ocamlbuild -clean
	./clean.sh
