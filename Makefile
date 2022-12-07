GHC ?= ghc
DEPS = .deps/xmonad-contrib .deps/xmonad
bin_name = xmonad-$(shell uname -m)-linux

bin: $(bin_name)

all: restart tags TAGS

restart: ./$(bin_name)
	./$< --restart

$(bin_name): $(wildcard *.hs)
	$(GHC) -O2 Main.hs -o $@

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
	rm -v *.errors *.hi *.o $(bin_name)

nix:
	nix-shell --pure --command make

.PHONY: restart all cabal clean deps bin
