webcam:
	g++ -fPIC -shared webcam.cpp -o webcam.dylib \
	`pkg-config --cflags --libs opencv4`

scheme:
	guile test.scm

build-go:
	go build -o main main.go
	install_name_tool -change @rpath/libapriltag.3.dylib /Users/sjoerd.dost/deosjr/apriltag/build/libapriltag.3.dylib main

run: build-go
	./main
	
