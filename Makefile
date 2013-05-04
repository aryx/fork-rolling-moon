#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################

LEVEL_SRC=\
   level.0.svgz \
   level.1.svgz \
   level.2.svgz \
   level.3.svgz \
   level.4.svgz \
   level.5.svgz \
   #E

LDATA := $(patsubst %.svgz, %.data, $(LEVEL_SRC))
LPLAY := $(patsubst %, -l %, $(LDATA))

INCLUDES=-I external/glMLite/SRC -I external/ocaml-chipmunk
LIBS=chipmunk.cma GL.cma Glu.cma Glut.cma  jpeg_loader.cma

##############################################################################
# Top rules
##############################################################################

# boot
all: rolling_moon

all.opt: rolling_moon.opt

rolling_moon: rolling_moon.ml
	ocamlc -annot $(INCLUDES) $(LIBS) $< -o $@

rolling_moon.opt: rolling_moon.ml
	ocamlopt -annot $(INCLUDES) $(LIBS:.cma=.cmxa) $< -o $@

# clean
clean:
	rm -f *~ *.cm[iox] *.o *.bin *.opt *.data

# levels
edit:
	inkscape $(LEVEL_SRC)

##############################################################################
# Levels
##############################################################################

level:  $(LDATA)
%.data: %.svgz
	sh mk-level-data.sh $< $@

##############################################################################
# Packaging
##############################################################################
