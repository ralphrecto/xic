SRCS  = $(shell find src -name '*.java')
TESTS =
LIBS  = # $(shell ls lib/*.jar | paste -d, -s)
CP    = $(LIBS):$$CLASSPATH
BIN   = bin
DOC   = doc

default: clean src test doc

.PHONY: src
src: $(SRCS)
	@echo "********************************************************************************"
	@echo "* make src                                                                     *"
	@echo "********************************************************************************"
	mkdir -p $(BIN) && javac -d $(BIN) -cp $(CP) $(SRCS)
	@echo

.PHONY: doc
doc:
	@echo "********************************************************************************"
	@echo "* make doc                                                                     *"
	@echo "********************************************************************************"
	mkdir -p $(DOC) && javadoc -Xdoclint:all,-missing -d $(DOC) -cp $(CP) $(SRCS)
	@echo

.PHONY: test
test: src
	@echo "********************************************************************************"
	@echo "* make test                                                                    *"
	@echo "********************************************************************************"
	java -cp $(BIN):$(CP) org.junit.runner.JUnitCore $(TESTS)
	@echo

.PHONY: publish
publish: doc
	@echo "********************************************************************************"
	@echo "* make publish                                                                 *"
	@echo "********************************************************************************"
	ghp-import -n doc
	git push -f origin gh-pages
	@echo

.PHONY: clean
clean:
	@echo "********************************************************************************"
	@echo "* make clean                                                                   *"
	@echo "********************************************************************************"
	rm -rf $(BIN)
	rm -rf $(DOC)
	@echo

print-%:
	@echo $*=$($*)
