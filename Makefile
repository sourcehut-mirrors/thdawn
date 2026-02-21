.PHONY: all
all:
	BUILD_VERSION=$$(git describe --always) ./build.ss

.PHONY: clean
clean:
	rm -f config.so coro.so funcutils.so geom.so \
		main.so main_wpo.so raylib.so srfi171.so srfi171meta.so \
		*.wpo *.boot
