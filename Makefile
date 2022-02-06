
terr: scanner.pl parser.pl
	swipl -g go -t halt --stand_alone=true -o terr -c $<

.PHONY: test
test:
	swipl -g "load_test_files([]),run_tests" -t halt scanner.pl parser.pl

.PHONY: clean
clean:
	rm -f terr
