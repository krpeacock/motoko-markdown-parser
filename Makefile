.PHONY: test

test:
	lib/moc -r $(shell mops sources) -wasi-system-api src/markdown/tests/Test.mo


watch:
	# watch changes in src/markdown and run tests
	# Path: Makefile
	# Usage: make watch
	# Dependencies: fswatch
	fswatch -o src/markdown | xargs -n1 -I{} make test
