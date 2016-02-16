.PHONY: all
.SUFFIXES: .html .asciidoc .ml

# asciidoc bootstrap backed we get from https://github.com/llaville/asciidoc-bootstrap-backend
# unpack archive to get ~/.asciidoc/backends/asciidoc/bootstrap/bootstrap.conf


TUTORIAL_DEMOS=
TUTORIAL2_DEMOS+=$(addprefix demos/0.3/demo1/,controller.ml Root.qml program.ml)
TUTORIAL2_DEMOS+=$(addprefix demos/0.3/demo2/,controller.ml Root.qml program.ml)
TUTORIAL2_DEMOS+=$(addprefix demos/0.3/demo3/,controller.ml Root.qml program.ml)
TUTORIAL2_DEMOS+=$(addprefix demos/0.3/demo4/,controller.ml Root.qml program.ml item.ml)

#$(warning $(TUTORIAL2_DEMOS))

OPTS=theme=readable totop=ui linkcss sidebar=left icons iconsfont=font-awesome # iconsfont=glyphicon

all: tutorial.html tutorial2.html #tutorial3.html

tutorial2.html: $(TUTORIAL2_DEMOS)

tutorial2.html: tutorial2.asciidoc
	asciidoc -b bootstrap $(addprefix -a ,$(OPTS)) $<

#tutorial3.html: tutorial3.asciidoc
#	asciidoc -b bootstrap $(addprefix -a ,$(OPTS)) $<

.asciidoc.html:
	asciidoc -b html5 -a icons -a toc2 -a theme=flask $<

celan: clean

clean:
	rm -f tutorial.html tutorial2.html

