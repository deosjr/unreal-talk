# Build assumes the AprilTag C library and the AprilTag tag-images repo
# live as siblings of this repo:
#
#   ~/deosjr/unreal-talk     <-- this repo
#   ~/deosjr/apriltag        <-- https://github.com/AprilRobotics/apriltag
#   ~/deosjr/apriltag-imgs   <-- https://github.com/AprilRobotics/apriltag-imgs
#
# The cgo directives in main.go reference `../apriltag` as a relative
# path, so cloning side-by-side is the only convention that works
# without editing source. Run `make setup-deps` once to fetch and build
# AprilTag.

APRILTAG_DIR := ../apriltag
APRILTAG_IMGS_DIR := ../apriltag-imgs
APRILTAG_BUILD := $(APRILTAG_DIR)/build

CGO_CFLAGS := $(shell guile-config compile)
# -Wl,-rpath bakes the AprilTag build directory into the binary as a
# runtime library search path. Replaces the previous install_name_tool
# step (which hardcoded a user-specific absolute path) on macOS; works
# on Linux too.
CGO_LDFLAGS := $(shell guile-config link) -Wl,-rpath,$(abspath $(APRILTAG_BUILD))
export CGO_CFLAGS
export CGO_LDFLAGS

# Detect platform-specific shared-library suffix.
UNAME := $(shell uname)
ifeq ($(UNAME),Darwin)
  SHLIB_SUFFIX := dylib
else
  SHLIB_SUFFIX := so
endif

# ---------------------------------------------------------------------
# One-shot dependency setup.
# ---------------------------------------------------------------------

setup-deps:
	@echo "==> cloning AprilTag siblings"
	@test -d $(APRILTAG_DIR) || git clone https://github.com/AprilRobotics/apriltag.git $(APRILTAG_DIR)
	@test -d $(APRILTAG_IMGS_DIR) || git clone https://github.com/AprilRobotics/apriltag-imgs.git $(APRILTAG_IMGS_DIR)
	@echo "==> building AprilTag"
	cmake -B $(APRILTAG_BUILD) -S $(APRILTAG_DIR)
	cmake --build $(APRILTAG_BUILD)

# ---------------------------------------------------------------------
# Build the OpenCV wrapper shared lib and the Go binary.
# ---------------------------------------------------------------------

build-opencv:
	g++ -std=c++17 -fPIC -shared -o libcvmatwrapper.$(SHLIB_SUFFIX) cvmat_wrapper.cpp `pkg-config --cflags --libs opencv4`

build-go:
	go build -o main .

# ---------------------------------------------------------------------
# Run / calibrate.
# ---------------------------------------------------------------------

# Homebrew installs Guile site modules (e.g. guile-gnutls) into
# /opt/homebrew/share/guile/site/3.0, but the Guile embedded into `main`
# uses Guile's compiled-in default load path which only sees its own
# Cellar. Point GUILE_*_PATH at the shared site dirs so HTTPS via
# (web client) -> (gnutls) actually resolves.
GUILE_SITE := /opt/homebrew/share/guile/site/3.0
GUILE_SITE_CCACHE := /opt/homebrew/lib/guile/3.0/site-ccache
GUILE_EXT := /opt/homebrew/lib/guile/3.0/extensions

run: build-opencv build-go
	GUILE_LOAD_PATH="$(GUILE_SITE)" \
	GUILE_LOAD_COMPILED_PATH="$(GUILE_SITE_CCACHE)" \
	GUILE_SYSTEM_EXTENSIONS_PATH="$(GUILE_EXT)" \
	./main

calibrate: build-opencv build-go
	rm -f calibration.json
	./main --calibrate

print: build-opencv build-go
	./main --print

# ---------------------------------------------------------------------
# Scheme unit tests.
# ---------------------------------------------------------------------

# Pure-Scheme tests (no OpenCV/AprilTag/camera needed). Each *_test.scm
# loads its target with (include) and exits non-zero on failure.
# GUILE_AUTO_COMPILE=0 is required: these files (include) their target
# module, and Guile's .go cache for the test file is NOT invalidated when
# only the included module changes — so a cached build would test stale code.
TEST_FILES := $(wildcard *_test.scm)

test:
	@status=0; \
	for t in $(TEST_FILES); do \
		echo "==> $$t"; \
		GUILE_AUTO_COMPILE=0 guile -L . "$$t" || status=1; \
	done; \
	exit $$status

.PHONY: setup-deps build-opencv build-go run calibrate test
