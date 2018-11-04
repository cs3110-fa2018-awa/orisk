MODULES=board_state game_state board player game display interface move heuristic authors
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
GAME=game.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,yojson,ANSITerminal,qcheck,str

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(GAME) && ./$(GAME)

bisect-test:
	$(OCAMLBUILD) -package bisect -syntax camlp4o,bisect_pp \
	  $(TEST) && ./$(TEST) -runner sequential

bisect: clean bisect-test
	bisect-report -I _build -html report bisect0001.out

zip:
	zip a678src.zip *.ml* *.json _tags Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report a678src.zip bisect*.out
