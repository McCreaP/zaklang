ZAKLANG_IMAGE_NAME = zaklang
ZAKLANG_IMAGE_TAG = 0.0.1

HOST_REPO_ROOT := $(shell pwd)
CONTAINER_REPO_ROOT = /mnt/zaklang

_build_deps_image:
	docker build --target deps -t $(ZAKLANG_IMAGE_NAME):$(ZAKLANG_IMAGE_TAG) .

_build_zaklang_image:
	docker build -t $(ZAKLANG_IMAGE_NAME):$(ZAKLANG_IMAGE_TAG) .

_build_dir:
	mkdir -p build

_bin_dir:
	mkdir -p bin

_frotend: _build_dir src/Zaklang.cf
	cd build \
	&& bnfc -haskell ../src/Zaklang.cf \
	&& happy -gca ParZaklang.y \
	&& alex -g LexZaklang.x

zaklang: _frotend _build_dir _bin_dir
	ghc -isrc:build -hidir build -odir build --make src/Interpreter.hs -o bin/zaklang

dev: _build_deps_image
	docker run --rm -it \
		-v $(HOST_REPO_ROOT):$(CONTAINER_REPO_ROOT) \
		-w $(CONTAINER_REPO_ROOT) \
		$(ZAKLANG_IMAGE_NAME):$(ZAKLANG_IMAGE_TAG) /bin/bash

interpret: _build_zaklang_image
	$(eval CONTAINER=$(shell docker create $(ZAKLANG_IMAGE_NAME):$(ZAKLANG_IMAGE_TAG) /tmp/input))
	docker cp $(INPUT) $(CONTAINER):/tmp/input
	docker start --attach $(CONTAINER)

clean:
	-rm -rf bin build
