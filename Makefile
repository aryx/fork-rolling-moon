#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################

LEVEL_SRC=\
   data/level.0.svgz \
   data/level.1.svgz \
   data/level.2.svgz \
   data/level.3.svgz \
   data/level.4.svgz \
   data/level.5.svgz \
   #E

LDATA := $(patsubst data/%.svgz, %.data, $(LEVEL_SRC))
LPLAY := $(patsubst data/%.svgz, -l %.data, $(LEVEL_SRC))

INCLUDES=-I external/glMLite/SRC -I external/ocaml-chipmunk
LIBS=chipmunk.cma GL.cma Glu.cma Glut.cma jpeg_loader.cma bigarray.cma

##############################################################################
# Top rules
##############################################################################

# boot
all: rolling_moon.opt moon_buggy.opt demo_dist.opt mk_level_data.byte levels

all.opt: rolling_moon.opt

rolling_moon: rolling_moon.ml
	ocamlc -annot $(INCLUDES) $(LIBS) $< -o $@

rolling_moon.opt: rolling_moon.ml
	ocamlopt -annot -bin-annot $(INCLUDES) $(LIBS:.cma=.cmxa) $< -o $@


mk_level_data.byte: mk_level_data.cmo
	ocamlfind ocamlc -package xml-light,str -linkpkg -o $@ $<

mk_level_data.cmo: mk_level_data.ml
	ocamlfind ocamlc -package xml-light -annot $< -c

levels:  $(LDATA)

%.data: data/%.svgz
	sh mk-level-data.sh $< $@

# clean
clean:
	rm -f *~ *.cm[iox] *.o *.bin *.opt *.data *.annot *.byte *.cmt

play:
	./rolling_moon.opt $(LPLAY)

edit:
	inkscape $(LEVEL_SRC)


##############################################################################
# Packaging
##############################################################################

##############################################################################
# Examples
##############################################################################

moon_buggy.opt: moon_buggy.ml
	ocamlopt.opt -annot -bin-annot $(INCLUDES) $(LIBS:.cma=.cmxa) $< -o $@

demo_dist.opt: demo_dist.ml
	ocamlopt.opt -annot -bin-annot $(INCLUDES) $(LIBS:.cma=.cmxa) $< -o $@

##############################################################################
# Developer rules
##############################################################################

graph:
	~/pfff/codegraph -symlinks -lang cmt -build .

visual:
	~/pfff/codemap -no_legend -profile -ss 2 -filter pfff .
