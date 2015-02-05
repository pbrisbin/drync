NAME = drync
VERSION = 0.2.0
RELEASE = 1

PREFIX    ?= /usr/local
MANPREFIX ?= $(PREFIX)/share/man

doc/drync.1: doc/drync.1.md
	kramdown-man < doc/drync.1.md > doc/drync.1
	[ -s doc/drync.1 ]

man: doc/drync.1

build:
	cabal build

package: build man
	mkdir -p $(NAME)-$(VERSION)/{bin,doc}
	cp Makefile $(NAME)-$(VERSION)/
	cp dist/build/$(NAME)/$(NAME) $(NAME)-$(VERSION)/bin/
	cp doc/drync.1 $(NAME)-$(VERSION)/doc/
	tar czvf pkg/$(NAME)-$(VERSION).tar.gz $(NAME)-$(VERSION)
	rm -rf $(NAME)-$(VERSION)

dist: package
	s3cmd --quiet --acl-public \
	  put pkg/$(NAME)-$(VERSION).tar.gz \
	  s3://source.pbrisbin.com/$(NAME)-$(VERSION).tar.gz

distclean:
	rm -f pkg/$(NAME)-$(VERSION).tar.gz

pkgver:
	sed -i "s/^pkgver=.*/pkgver=$(VERSION)/" pkg/PKGBUILD
	sed -i "s/^pkgrel=.*/pkgrel=$(RELEASE)/" pkg/PKGBUILD

pkgsums:
	(cd pkg && updpkgsums)
	rm -rf pkg/src pkg/pkg

release: dist pkgver pkgsums distclean

distcheck:
	(cd pkg && makepkg -s -c -i)
	rm -rf pkg/$(NAME)-*

install:
	install -Dm755 bin/drync $(DESTDIR)/$(PREFIX)/bin/drync
	install -Dm644 doc/drync.1 $(DESTDIR)/$(MANPREFIX)/man1/drync.1

uninstall:
	$(RM) $(DESTDIR)/$(PREFIX)/bin/drync \
	  $(DESTDIR)/$(MANPREFIX)/man1/drync.1

.PHONY: man build package dist distclean pkgver pkgsums release distcheck install uninstall
