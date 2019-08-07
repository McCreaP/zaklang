FROM debian:8.11

RUN apt-get update && apt-get install -y \
	make \
	bnfc \
	happy \
	alex \
	ghc \
	cabal-install \
	python3 \
	&& rm -rf /var/lib/apt/lists/*

RUN cabal update && cabal install --global mtl
