NAME = drync
VERSION = 0.0.1

PREFIX    ?= /usr/local
MANPREFIX ?= $(PREFIX)/share/man

build:
	cabal build

package:
	mkdir -p $(NAME)-$(VERSION)/bin
	cp Makefile $(NAME)-$(VERSION)/
	cp dist/build/$(NAME)/$(NAME) $(NAME)-$(VERSION)/bin/
	tar czvf $(NAME)-$(VERSION).tar.gz $(NAME)-$(VERSION)
	rm -rf $(NAME)-$(VERSION)

publish:
	scp $(NAME)-$(VERSION).tar.gz \
	  pbrisbin.com:/srv/http/source/$(NAME)-$(VERSION).tar.gz
	rm -rf $(NAME)-$(VERSION).tar.gz

release: build package publish

install:
	install -Dm755 bin/drync $(DESTDIR)/$(PREFIX)/bin/drync

uninstall:
	$(RM) $(DESTDIR)/$(PREFIX)/bin/drync

.PHONY: build package publish release install uninstall
