.PHONY: install

INSTALL_DIR = ${HOME}/.xmonad
LIB_DIR = $(INSTALL_DIR)/lib
ICONS_DIR = $(INSTALL_DIR)/icons

host := $(shell hostname)
ifeq ($(host), axolotl)
    conf = axolotl
else ifeq ($(host), echidna)
    conf = echidna
else
    conf = default
endif

xmonad = configs/$(conf).hs
lib = lib/DefaultConfig.hs lib/MyConfig.hs lib/Theme.hs
icons = $(wildcard icons/*.xpm)

install: build my-xmonad.cabal stack.yaml $(xmonad) $(lib) $(icons)
	mkdir -p $(INSTALL_DIR)
	install -m 644 $(xmonad) $(INSTALL_DIR)/xmonad.hs
	install -m 755 build $(INSTALL_DIR)/build
	install -m 644 my-xmonad.cabal $(INSTALL_DIR)/my-xmonad.cabal
	install -m 644 stack.yaml $(INSTALL_DIR)/stack.yaml
	mkdir -p $(LIB_DIR)
	install -m 644 $(lib) $(LIB_DIR)
	mkdir -p $(ICONS_DIR)
	install -m 644 $(icons) $(ICONS_DIR)
	xmonad --recompile
