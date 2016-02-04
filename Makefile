SRCS  = $(shell find src -name '*.java') $(shell find test -name '*.java')
TESTS = $(shell find test -name '*.java' \
            | sed 's/.java//' \
		    | sed 's_test/__' \
			| tr '/' '.')
LIBS  = $(shell ls lib/*.jar | paste -d: -s)
CP    = $(LIBS):$$CLASSPATH
BIN   = bin
DOC   = doc

JAVAC_FLAGS = -Xlint -Werror
JAVADOC_FLAGS = -Xdoclint:all,-missing

default: clean src test doc publish

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
	ghp-import -n doc
	git push -f origin gh-pages
	@echo

.PHONY: clean
clean:
	@echo "********************************************************************"
	@echo "* make clean                                                       *"
	@echo "********************************************************************"
	rm -rf $(BIN)
	rm -rf $(DOC)
	@echo

print-%:
	@echo $*=$($*)
