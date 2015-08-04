PKG_TARBALL = metrics.tar.gz

all byte native: configure
	ocaml setup.ml -build

configure: setup.ml
	ocaml $< -configure --enable-tests

setup.ml: _oasis
	oasis setup -setup-update dynamic

test doc install uninstall reinstall: all
	ocaml setup.ml -$@

clean:
	ocaml setup.ml -clean
	$(RM) $(PKG_TARBALL)
