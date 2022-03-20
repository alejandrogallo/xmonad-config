GHC ?= ghc
DEPS = .deps/xmonad-contrib .deps/xmonad

all: restart tags TAGS

restart: ./xmonad-x86_64-linux
	./$< --restart

xmonad-x86_64-linux: Main.hs
	$(GHC) -O2 $< -o $@

tags TAGS: $(DEPS)
	ctags -a -R $?
	ctags -e -a -R $?

.deps/xmonad:
	mkdir -p $(@D)
	git clone https://github.com/xmonad/xmonad $@

.deps/xmonad-contrib:
	mkdir -p $(@D)
	git clone https://github.com/xmonad/xmonad-contrib $@

deps: $(DEPS)

clean:
	rm -v *.errors *.hi *.o xmonad-x86_64-linux

nix:
	nix-shell --pure --command make

.PHONY: restart all cabal clean deps
