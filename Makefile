main:
	ocamlbuild -r -use-ocamlfind -pkgs oUnit,str,csv,gnuplot,ANSITerminal main.byte && ./main.byte

test:
	ocamlbuild -r -use-ocamlfind -pkgs oUnit,str,csv,gnuplot test.byte && ./test.byte

clean:
	ocamlbuild -clean
