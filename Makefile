# Convenience wrapper around lazbuild. See CLAUDE.md for details.
#   make            build everything
#   make client | server | launcher | ai | ai_c
#   make clean

LAZBUILD ?= $(HOME)/fpcupdeluxe/trunk/lazarus/lazbuild
SYNAPSE  ?= $(HOME)/projekte/synapse
WS       ?= gtk2

ROOT := $(CURDIR)
OPT  := --ws=$(WS) \
        --opt=-Fu$(ROOT)/client \
        --opt=-Fu$(ROOT)/client/sdl2 \
        --opt=-Fu$(ROOT)/client/sdl2/SDL2-for-Pascal/units \
        --opt=-Fu$(SYNAPSE)

.PHONY: all client server launcher ai ai_c clean

all: server ai client launcher

client:
	$(LAZBUILD) $(OPT) client/fpc_atomic.lpi

server:
	$(LAZBUILD) $(OPT) server/atomic_server.lpi

launcher:
	$(LAZBUILD) $(OPT) launcher/atomic_launcher.lpi

ai:
	$(LAZBUILD) $(OPT) ai/ai.lpi

ai_c:
	$(MAKE) -C ai_c

clean:
	rm -f fpc_atomic atomic_server atomic_launcher libai.so
	rm -rf client/lib/x86_64-linux server/lib/x86_64-linux launcher/lib/x86_64-linux ai/lib/x86_64-linux
