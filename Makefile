build-opencv:
	g++ -std=c++17 -fPIC -shared -o libcvmatwrapper.dylib cvmat_wrapper.cpp `pkg-config --cflags --libs opencv4`

build-go:
	go build -o main .
	install_name_tool -change @rpath/libapriltag.3.dylib /Users/sjoerd.dost/deosjr/apriltag/build/libapriltag.3.dylib main

run: build-opencv build-go
	export CGO_CFLAGS="$(shell guile-config compile)"
	export CGO_LDFLAGS="$(shell guile-config link)"
	./main

calibrate:
	go run calibrate.go
