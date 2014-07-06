.PHONY: all
.SUFFIXES: .html .asciidoc .ml

TUTORIAL_DEMOS=
TUTORIAL2_DEMOS+=$(addprefix demos/0.3/demo1/,controller.ml Root.qml program.ml)
TUTORIAL2_DEMOS+=$(addprefix demos/0.3/demo2/,controller.ml Root.qml program.ml)
TUTORIAL2_DEMOS+=$(addprefix demos/0.3/demo3/,controller.ml Root.qml program.ml)
TUTORIAL2_DEMOS+=$(addprefix demos/0.3/demo4/,controller.ml Root.qml program.ml)

#$(warning $(TUTORIAL2_DEMOS))

all: tutorial.html tutorial2.html

tutorial2.html: $(TUTORIAL2_DEMOS)

.asciidoc.html:
	asciidoc -b html5 -a icons -a toc2 -a theme=flask $<

