SRCDIR = src/mjw297
PKG = mjw297
PARSER_CLASS = Parser
SYMBOL_CLASS = Sym
LEXER_CLASS = Lexer
PARSER = $(SRCDIR)/$(PARSER_CLASS)
SYMBOL = $(SRCDIR)/$(SYMBOL_CLASS)
LEXER  = $(SRCDIR)/$(LEXER_CLASS)
CUPJAR = lib/java_cup.jar
FLEXJAR = lib/jflex-1.6.1.jar
SRCS  = $(shell find src -name '*.java') \
        $(shell find test -name '*.java') \
	    $(LEXER).java \
	    $(PARSER).java \
	    $(SYMBOL).java
TESTS = $(shell find test -name '*Test.java' \
            | sed 's/.java//' \
		    | sed 's_test/__' \
			| tr '/' '.')
LIBS  = $(shell ls lib/*.jar | tr '\n' ':' | sed 's/:$$//')
CP    = $(LIBS):$$CLASSPATH
BIN   = bin
DOC   = doc

JAVAC_FLAGS = -Xlint
JAVADOC_FLAGS = -Xdoclint:all,-missing

default: clean src test doc publish

$(LEXER).java: $(LEXER).jflex
	java -jar $(FLEXJAR) $<

$(PARSER).java $(SYMBOL).java: $(PARSER).cup
	java  -cp $(CUPJAR):$(CP) java_cup.Main \
		                -destdir $(SRCDIR) \
		                -package $(PKG) \
						-parser $(PARSER_CLASS) \
						-symbols $(SYMBOL_CLASS) \
						-nowarn \
						$<

.PHONY: src
src: $(SRCS)
	@echo "********************************************************************"
	@echo "* make src                                                         *"
	@echo "********************************************************************"
	mkdir -p $(BIN) && javac $(JAVAC_FLAGS) -d $(BIN) -cp $(CP) $(SRCS)
	@echo

.PHONY: doc
doc:
	@echo "********************************************************************"
	@echo "* make doc                                                         *"
	@echo "********************************************************************"
	mkdir -p $(DOC) && javadoc $(JAVADOC_FLAGS) -d $(DOC) -cp $(CP) $(SRCS)
	@echo

.PHONY: test
test: src
	@echo "********************************************************************"
	@echo "* make test                                                        *"
	@echo "********************************************************************"
	java -cp $(BIN):$(CP) org.junit.runner.JUnitCore $(TESTS)
	@echo

.PHONY: publish
publish: doc
	@echo "********************************************************************"
	@echo "* make publish                                                     *"
	@echo "********************************************************************"
	# http://stackoverflow.com/a/677212/3187068
	if command -v ghp-import >/dev/null 2>&1; then \
		ghp-import -n doc; \
		git push -f origin gh-pages; \
	else \
		echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"; \
		echo "! ghp-import not installed; docs will not be published             !"; \
		echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"; \
	fi
	@echo

.PHONY: clean
clean:
	@echo "********************************************************************"
	@echo "* make clean                                                       *"
	@echo "********************************************************************"
	rm -rf $(BIN)
	rm -rf $(DOC)
	rm -rf $(PARSER).java
	rm -rf $(SYMBOL).java
	rm -rf $(LEXER).java
	rm -f  p1.zip
	@echo

p1.zip: $(LEXER).java $(SYMBOL).java $(PARSER).java src lib xic bin test doc Makefile README.md vagrant xic-build
	zip -r $@ $^

print-%:
	@echo $*=$($*)
