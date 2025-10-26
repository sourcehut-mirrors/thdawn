.PHONY: all
all:
	./build.ss

.PHONY: clean
clean:
	rm -f config.so coro.so funcutils.so geom.so \
		main.so main_wpo.so raylib.so *.wpo *.boot
