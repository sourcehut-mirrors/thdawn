#!/usr/bin/env sh
sbcl --load 'thdawn.asd' \
	 --eval "(ql:quickload 'thdawn)" \
	 --eval "(sb-ext:save-lisp-and-die #p\"thdawn\" \
	 		:toplevel #'thdawn:main :executable t)"
