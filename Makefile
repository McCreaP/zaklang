UID := $(shell id -u)
GID := $(shell id -g)

ZAKLANG_IMAGE_NAME = zaklang
ZAKLANG_IMAGE_TAG = 0.0.1

HOST_REPO_ROOT := $(shell pwd)
CONTAINER_REPO_ROOT = /mnt/zaklang

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
	ghc -isrc:build -hidir build -odir build --make src/Main.hs -o bin/zaklang

printTypes: _frotend _build_dir _bin_dir
	ghc -isrc:build -hidir build -odir build --make test/PrintTypes.hs -o bin/printTypes

test: zaklang
	test/runTest.py

_build_dev_image:
	docker build -t $(ZAKLANG_IMAGE_NAME):$(ZAKLANG_IMAGE_TAG) .

dev: _build_dev_image
	docker run --rm -it \
		-u $(UID):$(GID) \
		-v $(HOST_REPO_ROOT):$(CONTAINER_REPO_ROOT) \
		-w $(CONTAINER_REPO_ROOT) \
		$(ZAKLANG_IMAGE_NAME):$(ZAKLANG_IMAGE_TAG) /bin/bash

clean:
	-rm -rf bin build
