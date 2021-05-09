
.PHONY: all clean

DIR:=svg-src
FILES:=$(patsubst $(DIR)/%.svg,%.svg,$(wildcard $(DIR)/*.svg))

all: $(FILES)
clean:
	rm -f $(FILES)

%.svg: $(DIR)/%.svg
	cabal exec import-svg  < $^ > $@
