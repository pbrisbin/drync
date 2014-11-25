NAME = drync
VERSION = 0.0.2
RELEASE = 1

PREFIX    ?= /usr/local
MANPREFIX ?= $(PREFIX)/share/man

build:
	cabal build

package:
	mkdir -p $(NAME)-$(VERSION)/bin
	cp Makefile $(NAME)-$(VERSION)/
	cp dist/build/$(NAME)/$(NAME) $(NAME)-$(VERSION)/bin/
	tar czvf pkg/$(NAME)-$(VERSION).tar.gz $(NAME)-$(VERSION)
	rm -rf $(NAME)-$(VERSION)

dist: build package
	scp pkg/$(NAME)-$(VERSION).tar.gz \
	  pbrisbin.com:/srv/http/source/$(NAME)-$(VERSION).tar.gz

distclean:
	rm -f pkg/$(NAME)-$(VERSION).tar.gz

pkgver:
	sed -i "s/^pkgver=.*/pkgver=$(VERSION)/" pkg/PKGBUILD
	sed -i "s/^pkgrel=.*/pkgrel=$(RELEASE)/" pkg/PKGBUILD

pkgsums:
	(cd pkg && updpkgsums)
	rm -rf pkg/src pkg/pkg

release: build package dist pkgver pkgsums distclean

distcheck:
	(cd pkg && makepkg -s -c -i)
	rm -rf pkg/$(NAME)-*

install:
	install -Dm755 bin/drync $(DESTDIR)/$(PREFIX)/bin/drync

uninstall:
	$(RM) $(DESTDIR)/$(PREFIX)/bin/drync

.PHONY: build package dist distclean pkgver pkgsums release distcheck install uninstall
