online:
	sbcl --load build-online.lisp "$@"

offline:
	sbcl --load build-offline.lisp "$@"

clean:
	rm -f online
	rm -f offline
