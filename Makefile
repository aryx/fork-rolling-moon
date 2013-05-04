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

CHIP_PATH := +chipmunk

# boot
all: run-opt

run-int: rolling_moon.ml $(LDATA)
	./$< $(LPLAY)

run-bin: rolling_moon.bin $(LDATA)
	./$< $(LPLAY)

run-opt: rolling_moon.opt $(LDATA)
	./$< $(LPLAY)

# levels
edit:
	inkscape $(LEVEL_SRC)

level:  $(LDATA)
%.data: %.svgz
	sh mk-level-data.sh $< $@

# compilations
bin: rolling_moon.bin
opt: rolling_moon.opt

rolling_moon_.ml: rolling_moon.ml
	sed -e 's|^#.*$$||g' $< > $@

rolling_moon.bin: rolling_moon_.ml
	ocamlc  \
	  -I $(CHIP_PATH) chipmunk.cma  \
	  -I +glMLite GL.cma Glu.cma Glut.cma  \
	  jpeg_loader.cma \
	  $< -o $@

rolling_moon.opt: rolling_moon_.ml
	ocamlopt  \
	  -I $(CHIP_PATH) chipmunk.cmxa  \
	  -I +glMLite GL.cmxa Glu.cmxa Glut.cmxa  \
	  jpeg_loader.cmxa \
	  $< -o $@

# clean
clean:
	rm -f *~ *.cm[iox] *.o *.bin *.opt *.data _rolling-moon.ml

# tarball
VERSION=`date --iso`
DIR=rolling_moon-$(VERSION)
FILES=\
     README.html \
     Makefile \
     mk-level-data.ml \
     mk-level-data.sh \
     rolling_moon.ml \
     tex-b-64.jpg \
     best-times.txt \
     $(LEVEL_SRC)

LICENCE_GPL.txt:
	wget http://www.gnu.org/licenses/gpl-3.0.txt
	mv gpl-3.0.txt $@

pack: $(FILES) LICENCE_GPL.txt
	if [ ! -d $(DIR) ]; then mkdir $(DIR); fi
	cp $(FILES) $(DIR)/
	mv LICENCE_GPL.txt $(DIR)/
	tar cf $(DIR).tar  $(DIR)/
	if [ -f $(DIR).tar.bz2 ]; then rm -f $(DIR).tar.bz2 ; fi
	bzip2 -9  $(DIR).tar
	ls -lh  $(DIR).tar.bz2

