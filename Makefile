MODULES=authors main commands gitTree util parse
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
TEST_CMD=test_commands.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,unix,yojson,str

default: build
	utop
	
build: 
	$(OCAMLBUILD) $(OBJECTS) $(MAIN)

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

test-cmd:
	$(OCAMLBUILD) -tag debug $(TEST_CMD) && ./$(TEST_CMD)

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private .git-ml search_src.zip

zip:
	zip git-ml_src.zip *.ml* _tags Makefile

rebuild: clean build