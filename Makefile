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

run: build-opencv build-go
	./main

calibrate: build-opencv build-go
	rm -f calibration.json
	./main --calibrate
