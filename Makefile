INSTALL_DIR = ${HOME}/.xmonad
LIB_DIR = $(INSTALL_DIR)/lib
ICONS_DIR = $(INSTALL_DIR)/icons

RWX_INSTALL = mkdir -p $(@D) && install -m 755 $< $@
RW_INSTALL = mkdir -p $(@D) && install -m 644 $< $@

host := $(shell hostname)
ifeq ($(host), axolotl)
    conf = axolotl
else ifeq ($(host), echidna)
    conf = echidna
else
    conf = default
endif

xmonad = $(INSTALL_DIR)/xmonad.hs
cabal = $(INSTALL_DIR)/my-xmonad.cabal
stack = $(INSTALL_DIR)/stack.yaml
build = $(INSTALL_DIR)/build
bin = $(INSTALL_DIR)/xmonad-x86_64-linux

.PHONY: all
all: $(xmonad) $(cabal) $(stack) $(build) icons lib

$(xmonad): configs/$(conf).hs
	$(RW_INSTALL)
$(cabal): my-xmonad.cabal
$(stack): stack.yaml
$(build): build
	$(RWX_INSTALL)
icons: $(foreach x,$(wildcard icons/*.xpm),$(INSTALL_DIR)/$(x))
lib  : $(foreach x,$(wildcard lib/*.hs),   $(INSTALL_DIR)/$(x))

$(INSTALL_DIR)/%: %
	$(RW_INSTALL)

$(ICONS_DIR)/%.xpm: icons/%.xpm
	$(RW_INSTALL)

$(LIB_DIR)/%.hs: lib/%.hs
	$(RW_INSTALL)
