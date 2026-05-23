# unreal-talk

Second attempt at building a Dynamicland style environment.

## Install (macOS)

The build glues four ecosystems together: Go for the main binary, Guile
for the RealTalk runtime, OpenCV for the camera and image pipeline, and
the C AprilTag library for phyiscal tags. Each needs to be in
place before `make run` will succeed.

### 1. Toolchain prerequisites

```sh
brew install go guile guile-gnutls pkg-config opencv cmake
```

- **go** &mdash; main-binary toolchain. `go.mod` pins module mode; the
  Go version itself can be anything reasonably recent (the source uses
  1.24+ features as of this writing).
- **guile** &mdash; provides `libguile` (linked into the Go binary via
  cgo) and `guile-config`, which the Makefile shells out to for
  `CGO_CFLAGS` / `CGO_LDFLAGS`.
- **guile-gnutls** &mdash; Guile bindings for GnuTLS. Required for the
  HTTPS fetches in `webclient.scm`; without it, `(web client)`'s
  HTTPS path throws `gnutls-not-available` in a background thread and
  pages that fetch from the web stay stuck on LOADING.
- **pkg-config** &mdash; resolves OpenCV's compile and link flags for
  the C++ wrapper.
- **opencv** &mdash; large install (~1 GB after deps). Provides both
  the C++ headers and the dylibs that gocv binds to.
- **cmake** &mdash; needed to build AprilTag.

### 2. AprilTag + tag images

The repo expects AprilTag to live as a sibling, because `main.go` has
hardcoded relative cgo directives (`#cgo CFLAGS: -I../apriltag`). Same
for the tag-image PNGs (`print_tag.go` reads from `../apriltag-imgs`).

Clone this repo into `~/deosjr/unreal-talk` (or anywhere, as long as
the siblings line up), then:

```sh
make setup-deps
```

That clones [AprilRobotics/apriltag](https://github.com/AprilRobotics/apriltag)
and [AprilRobotics/apriltag-imgs](https://github.com/AprilRobotics/apriltag-imgs)
into `../apriltag` and `../apriltag-imgs`, runs `cmake -B build -S .`
inside the apriltag clone, and builds it. The result is
`../apriltag/build/libapriltag.3.dylib`, which the Go build links
against.

### 3. Build the OpenCV wrapper and main binary

```sh
make build-opencv   # builds libcvmatwrapper.dylib
make build-go       # builds ./main
```

`build-opencv` compiles `cvmat_wrapper.cpp` into a shared library that
Guile loads via FFI to talk to `cv::Mat`. `build-go` produces the main
binary; the runtime AprilTag dylib lookup is baked in via `-Wl,-rpath`
to `../apriltag/build`, so the binary finds its libs automatically with
no `DYLD_LIBRARY_PATH` shimming.

### 4. Camera permissions

First run will trigger a macOS dialog asking Terminal (or whichever
terminal app you use) for camera access. Approve. If you miss the
dialog and the camera silently fails, check System Settings &rarr;
Privacy & Security &rarr; Camera.

Many webcams ship with auto-exposure, auto-white-balance, and
auto-focus enabled, which are murder for stable tag detection. If your
camera has manufacturer software (e.g. Logitech G HUB), use it to lock
exposure / WB / focus to fixed values. The `webcam.Set(...)` calls in
`main.go` attempt the same thing via OpenCV, but in practice many
drivers ignore them.

The same goes for resolution. Calibration computes a homography that
maps webcam pixels to projector pixels; if the camera serves a
different resolution at runtime than it did during calibration, every
detected tag projects to scaled coordinates and silently fails the
bounds check at `main.go`'s `bound.In(...)`. Pin the resolution at the
driver level (Logitech G HUB &rarr; Camera Settings &rarr; Resolution,
or your camera's equivalent) to the value you'll calibrate at, and
leave it there. The default this codebase expects is **1280&times;720**.

### 5. HTTPS and Guile module paths

HTTPS fetches in `webclient.scm` shell out to **`curl`** rather than
going through Guile's `(web client)` &rarr; `(gnutls)` path. Three
reasons it's painful to use Guile-side HTTPS on macOS:

1. Homebrew installs Guile site modules (including `guile-gnutls`)
   under `/opt/homebrew/share/guile/site/3.0`, but the Guile runtime
   embedded into `./main` uses Guile's compiled-in default load path,
   which only sees its own Cellar. The Makefile's `run` target works
   around this by exporting `GUILE_LOAD_PATH`, `GUILE_LOAD_COMPILED_PATH`,
   and `GUILE_SYSTEM_EXTENSIONS_PATH` &mdash; if you ever do want
   Guile-side HTTPS, those env vars at the top of the Makefile are
   what you'd tweak.
2. GnuTLS doesn't read the macOS Keychain, so even with `guile-gnutls`
   loaded, certificate validation fails with `signer-not-found`.
3. `(web client)` doesn't expose any public hook for setting a custom
   trust file, so configuring a CA bundle requires monkey-patching.

`curl` sidesteps all of that. It's preinstalled on macOS, uses the
system Keychain (or its own CA bundle), and handles redirects via
`-L`. We pay for it with a `fork`/`exec` per fetch &mdash; fine for
infrequent wiki lookups, would be worth reconsidering for high-rate
use.

## Running

### Calibrate (one-time, per camera/projector setup)

```sh
make calibrate
```

This pops up a "Chessboard" window showing a 16&times;9 chessboard
pattern; press **f** to fullscreen it on the projector. The system
detects the corners through the camera and writes a homography to
`calibration.json`. Re-run whenever the camera or projector physically
moves.

### Live run

```sh
make run
```

Pop the projection window onto the projector display, fullscreen with
**f**. Place tagged pages (printed from `../apriltag-imgs/tagStandard41h12/`)
in the camera's view. Each tag's ID is looked up against `scripts/<id>.scm`
and that file's code becomes the active page logic; remove the tag and
the page's claims and rules are retracted.

## Design

Golang is the main entry point. It uses OpenCV to interface with the
camera and to draw windows. Go uses AprilTags via cgo to detect tags
and sends information about them to Guile. Guile runs RealTalk and
manages the Claim/Wish/When model. At start-up Go sends an image
pointer to Guile to draw on.

Each interaction between Go and Guile is one full Datalog fixpoint run.
During the fixpoint Guile draws on the image buffer. At the end of
that interaction Guile returns from the cgo function call; Go then
blits the image to the projection window.

At some point this might be simplified by only using Guile Scheme, but
perhaps having Go will be useful when we consider multiple machines
running multiple RealTalk instances.

## Log

* 23-05-2026: clean up nested Claims behaviour, removing Claim-derived
* 02-08-2025: async fetching urls from within Guile Scheme using threads
* 31-07-2025: datalog inefficiency starts to hurt; multiple rules get slow quite fast
* 30-07-2025: First live-edit demo is now working
* 30-07-2025: Using Logitech software to manually set webcam config is a drastic improvement
* 29-07-2025: Rough first implementation of memories using Remember/Forget
* 28-07-2025: Claim-derived as temporary fix for Claim-within-When bugs
* 27-07-2025: Guile openCV bindings are getting painful to write, need better wrappers
* 26-07-2025: Parallelizing tag parsing speeds things up dramatically
* 26-07-2025: Simple logging shows: tags flickering causes increasing lag in Scheme (fixed)
* 25-07-2025: Current setup can recognize 4x4cm tags stably
* 25-07-2025: Use the file system as database for page scripts
* 24-07-2025: Claim and When fact order is now in sync
* 23-07-2025: RealTalk ported from Hoot, draw routine triggered from fixpoint
* 22-07-2025: Go sends Guile a cv::Mat pointer, Guile draws on it, Go shows it
* 21-07-2025: Guile Scheme can call opencv through a C++ wrapper
* 21-07-2025: Golang talks to Guile Scheme using CGO directly now
* 21-07-2025: homography working and stable. for 3d look into PnPRansac
* 20-07-2025: gocv FileNode.Mat() is not implemented so we work around it for now
* 20-07-2025: chessboard calibration + hardcoded homography detection
* 19-07-2025: got Go working with gocv for webcam and CGO for apriltags
* 19-07-2025: gave up on FFI from Guile Scheme -> openCV + AprilTags for now
* 19-07-2025: evaluated QR-code detection in openCV, deemed not good enough
