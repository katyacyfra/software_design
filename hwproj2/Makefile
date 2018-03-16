all: uml.png

view: uml.png
	feh -. $<

%.png: %.dot
	dot $< -Tpng -o $@

clean:
	uml.png

.PHONY: all clean view
