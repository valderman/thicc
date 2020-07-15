.PHONY: build deb dummy
DEBDIR = _deb
DEBUILDDIR = $(DEBDIR)/build
PACKAGEFILES = _build debian README.md config thicc.service

dummy:
	@echo "Use 'cabal build' instead."

build:
	rm -r _build ; true
	mkdir -p _build
	cabal install --install-method=copy --installdir=_build
	strip -s _build/*

deb: build
	mkdir -p $(DEBUILDDIR)
	cp -rf $(PACKAGEFILES) $(DEBUILDDIR)
	cp -f LICENSE $(DEBUILDDIR)/debian/copyright
	cd $(DEBUILDDIR) && debuild -us -uc -b
	mv $(DEBDIR)/*.deb ./
	rm -r $(DEBDIR)
