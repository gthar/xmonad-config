INSTALL_DIR = ${HOME}/.xmonad
LIB_DIR = $(INSTALL_DIR)/lib
ICONS_DIR = $(INSTALL_DIR)/icons

RWX_INSTALL = mkdir -p $(@D) && install -m 755 $< $@
RW_INSTALL = mkdir -p $(@D) && install -m 644 $< $@

xmonad = $(INSTALL_DIR)/xmonad.hs
cabal = $(INSTALL_DIR)/my-xmonad.cabal
stack = $(INSTALL_DIR)/stack.yaml
build = $(INSTALL_DIR)/build

.PHONY: all
all: $(xmonad) $(cabal) $(stack) $(build) icons lib

$(build): build
	$(RWX_INSTALL)

$(xmonad): xmonad.hs
$(cabal): my-xmonad.cabal
$(stack): stack.yaml

icons: $(foreach x,$(wildcard icons/*.xpm),$(INSTALL_DIR)/$(x))
lib  : $(foreach x,$(wildcard lib/*.hs),   $(INSTALL_DIR)/$(x))

$(INSTALL_DIR)/%: %
	$(RW_INSTALL)

$(ICONS_DIR)/%.xpm: icons/%.xpm
	$(RW_INSTALL)

$(LIB_DIR)/%.hs: lib/%.hs
	$(RW_INSTALL)
