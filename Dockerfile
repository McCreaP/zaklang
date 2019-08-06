FROM debian:8.11 as deps

RUN apt-get update && apt-get install -y \
	make \
	bnfc \
	happy \
	alex \
	ghc \
	cabal-install \
	&& rm -rf /var/lib/apt/lists/*

RUN cabal update && cabal install mtl

FROM deps as builder

WORKDIR /opt/zaklang
COPY . /opt/zaklang
RUN make zaklang

FROM debian:8.11

COPY --from=builder /opt/zaklang/zaklang /usr/local/bin
ENTRYPOINT ["zaklang"]
