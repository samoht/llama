include config/Makefile

COBJS=open.o draw.o fill.o color.o text.o \
  image.o make_img.o dump_img.o point_col.o sound.o events.o subwindow.o
CAMLOBJS=graphics.cmo graphicsX11.cmo

all: graphics.cma libgraphics.a

graphics.cma: $(CAMLOBJS)
	llamamklib -o graphics -oc graphics -linkall $(CAMLOBJS) -cclib "\"$(X11_LINK)\""

libgraphics.a: $(COBJS)
	llamamklib -oc graphics $(COBJS) -ldopt "$(X11_LINK)"

%.o: %.c
	llamac -c -ccopt $(X11_INCLUDES) $<

%.cmi: %.mli
	llamac -c $<

%.cmo: %.ml
	llamac -c $<

install:
	cp libgraphics.a dllgraphics.so graphics.cma *.cmi *.mli $(LIBDIR)
.PHONY: install

clean:
	rm -f *.o *.a *.so *.cmi *.cmo *.cma
.PHONY: clean

depend:
	gcc -MM -I xxx *.c | sed -e 's, /xxx[^ ]*\.h,,g' > .depend
	llamadep *.mli *.ml >> .depend
.PHONY: depend

# Dependencies

color.o: color.c libgraph.h
draw.o: draw.c libgraph.h
dump_img.o: dump_img.c libgraph.h image.h
events.o: events.c libgraph.h
fill.o: fill.c libgraph.h
image.o: image.c libgraph.h image.h
make_img.o: make_img.c libgraph.h image.h
open.o: open.c libgraph.h
point_col.o: point_col.c libgraph.h
sound.o: sound.c libgraph.h
subwindow.o: subwindow.c libgraph.h
text.o: text.c libgraph.h
graphics.cmi:
graphicsX11.cmi:
graphics.cmo: graphics.cmi
graphics.cmx: graphics.cmi
graphicsX11.cmo: graphics.cmi graphicsX11.cmi
graphicsX11.cmx: graphics.cmx graphicsX11.cmi
