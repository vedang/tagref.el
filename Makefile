.PHONY: test check check-compile check-doc check-package-lint format clean all deps

EMACS ?= emacs
DEPS_DIR = .deps

all: check test

# Install test dependencies
deps:
	@mkdir -p $(DEPS_DIR)
	@if [ ! -d "$(DEPS_DIR)/buttercup" ]; then \
		echo "Installing buttercup..."; \
		git clone --depth 1 https://github.com/jorgenschaefer/emacs-buttercup.git $(DEPS_DIR)/buttercup; \
	fi
	@if [ ! -d "$(DEPS_DIR)/package-lint" ]; then \
		echo "Installing package-lint..."; \
		git clone --depth 1 https://github.com/purcell/package-lint.git $(DEPS_DIR)/package-lint; \
	fi

# Run Buttercup tests
test: deps
	$(EMACS) --batch \
		-L . \
		-L $(DEPS_DIR)/buttercup \
		-l buttercup \
		-l tagref.el \
		-l test/tagref-test.el \
		-f buttercup-run

# Run linting: byte-compile, checkdoc, and package-lint
check: check-compile check-doc check-package-lint

check-package-lint: deps
	$(EMACS) --batch \
		-L . \
		-L $(DEPS_DIR)/package-lint \
		-l package-lint \
		-f package-lint-batch-and-exit tagref.el

check-compile:
	$(EMACS) --batch \
		-L . \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile tagref.el

check-doc:
	$(EMACS) --batch \
		-L . \
		-l tagref.el \
		--eval "(checkdoc-file \"tagref.el\")"

# Format elisp (basic indentation check)
format:
	$(EMACS) --batch \
		-L . \
		-l tagref.el \
		--eval "(find-file \"tagref.el\")" \
		--eval "(indent-region (point-min) (point-max))" \
		--eval "(save-buffer)"

clean:
	rm -f *.elc
	rm -rf $(DEPS_DIR)
