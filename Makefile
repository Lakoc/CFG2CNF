TARGET := flp21-fun
ARCHIVE := flp-fun-xpolok03

.PHONY: build
build: $(TARGET)

$(TARGET): src/*.hs
	ghc --make Main.hs -o $@ -i $^ -Wall -fforce-recomp

.PHONY: arch
arch: $(ARCHIVE).zip

$(ARCHIVE).zip: Makefile README.md Main.hs src/*.hs tests
	zip -r $@ $^

.PHONY: test
test: $(TARGET)
	python tests/bad_arguments_test.py $^
	python tests/errors_test.py $^ tests/errors
	python tests/valid_inputs_test.py $^ tests/dumpCFG
	python tests/valid_inputs_test.py $^ tests/simplifyCFG
	python tests/valid_inputs_test.py $^ tests/CFG2CNF


.PHONY: clean
clean:
	rm -f $(ARCHIVE).zip *.hi *.o src/*.hi src/*.o $(TARGET)