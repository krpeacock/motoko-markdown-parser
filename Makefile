.PHONY: test test_linux

test:
	lib/darwin/moc -r $(shell mops sources) -wasi-system-api src/markdown/tests/Test.mo

test_linux:
	lib/linux/moc -r $(shell mops sources) -wasi-system-api src/markdown/tests/Test.mo

test_utils_linux:
	lib/linux/moc -r $(shell mops sources) -wasi-system-api src/markdown/tests/Utils.spec.mo


watch:
	# watch changes in src/markdown and run tests
	# Path: Makefile
	# Usage: make watch
	# Dependencies: fswatch
	fswatch -o src/markdown | xargs -n1 -I{} make test

watch_linux:
	fswatch -o src/markdown | xargs -n1 -I{} make test_linux
