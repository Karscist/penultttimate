# Parent Makefile

PREFIX = /usr/local
RAYLIB_DIR = raylib
RAYGUI_DIR = raygui
CLAW_DIR = claw-raylib
RAYGUI_MAKEFILE = $(RAYGUI_DIR)/src/Makefile
RAYLIB_FLAGS = PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED
LISP ?= sbcl --dynamic-space-size 4096

.PHONY: all raylib raygui install uninstall clean

all: raylib raygui claw-install

raylib:
	$(MAKE) $(RAYLIB_FLAGS) -C $(RAYLIB_DIR)/src

raylib-install:
	$(MAKE) $(RAYLIB_FLAGS) -C $(RAYLIB_DIR)/src install

raygui:
	cp rayguiMakefile $(RAYGUI_MAKEFILE)
	$(MAKE) -C $(RAYGUI_DIR)/src SHARED=$(SHARED)

raygui-install:
	cp rayguiMakefile $(RAYGUI_MAKEFILE)
	 $(MAKE) -C $(RAYGUI_DIR)/src/ install

claw-install:
	$(LISP) --load build.lisp --eval "(exit)"
	chmod 777 $(CLAW_DIR)/lib/*.so

install: raylib-install raygui-install claw-install

uninstall:

	 $(MAKE) -C $(RAYGUI_DIR)/src/ uninstall
	 $(MAKE) -C $(RAYLIB_DIR)/src/ uninstall
clean:
	$(MAKE) -C $(RAYLIB_DIR)/src/ clean || true
	$(MAKE) -C $(RAYGUI_DIR)/src/ clean || true
