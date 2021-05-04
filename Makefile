SOURCE := kindle-highlights-to-org.el
TEST   := ./test/kindle-highlights-to-org-test.el

.PHONY: test

test:
	cask exec buttercup -L .

testWatch:
	while true; do \
		make test; \
		inotifywait -qre close_write .; \
	done

lint:
	cask exec elsa $(SOURCE)
