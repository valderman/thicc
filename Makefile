.PHONY: build deb dummy

dummy:

build:
	rm -r _build ; true
	mkdir -p _build
	cabal install --install-method=copy --installdir=_build
	strip -s _build/*

deb: build
	cp -f LICENSE debian/copyright
	debuild -us -uc -b