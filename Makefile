LIB := -syntax camlp4o -package threads,netstring,calendar,extlib,postgresql,eliom.server,lwt.syntax
CAMLC := ocamlfind ocamlc -thread -g $(LIB)
CAMLOPT := ocamlfind ocamlopt -thread  $(LIB)
CAMLDOC := ocamlfind ocamldoc $(LIB)
CAMLDEP := ocamlfind ocamldep $(LIB)
CAMLBUILDOPTS := -ocamlc '$(CAMLC)' -ocamlopt '$(CAMLOPT)' -ocamldep '$(CAMLDEP)'
CAMLBUILD := ocamlbuild $(CAMLBUILDOPTS)

CMA := nurpawiki.cma
CMXA := nurpawiki.cmxa
CMXS := nurpawiki.cmxs

TARGETS := $(CMA)
ifneq ($(shell which ocamlopt),)
  TARGETS += $(CMXA)
  ifneq ($(wildcard $(shell ocamlc -where)/dynlink.cmxa),)
    TARGETS += $(CMXS)
  endif
endif

all: $(TARGETS) META

$(CMA): version.ml
	$(CAMLBUILD) -classic-display -ocamlc '$(CAMLC)' $@

$(CMXA): version.ml
	$(CAMLBUILD) -classic-display -ocamlopt '$(CAMLOPT)' $@

%.cmxs: %.cmxa
	$(CAMLOPT) -shared -linkall -o _build/$@ _build/$<

.PHONY: $(CMA) doc install

NWIKI_VER=$(shell cat VERSION)
version.ml:version.ml.in VERSION
	echo $(NWIKI_VER)
	sed -e "s|%_NURPAWIKI_VERSION_%|$(NWIKI_VER)|g" version.ml.in > version.ml

META:META.in VERSION
	sed -e "s|%_NURPAWIKI_VERSION_%|$(NWIKI_VER)|g" META.in > META

doc:
#	$(CAMLDOC) -d doc -html db.mli

clean:
	-rm -Rf _build META version.ml

install:
	ocamlfind install nurpawiki META $(foreach T,$(TARGETS),_build/$(T) $(if $(findstring .cmxa,$(T)),_build/$(T:.cmxa=.a)))
