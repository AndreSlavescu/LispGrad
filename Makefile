# Makefile for setting up Quicklisp and Lisp environment

# Quicklisp target
ql:
	@echo "Creating local_cache directory..."
	@mkdir -p local_cache
	@if [ ! -f $$HOME/quicklisp/setup.lisp ]; then \
		echo "Downloading Quicklisp installer to local_cache directory..."; \
		curl -o local_cache/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp; \
		echo "Installing Quicklisp..."; \
		sbcl --non-interactive --load local_cache/quicklisp.lisp \
			--eval "(quicklisp-quickstart:install)" \
			--eval "(ql:add-to-init-file)"; \
	else \
		echo "Quicklisp is already installed."; \
	fi
	@echo "Quicklisp installed successfully."
