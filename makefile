online:
	sbcl --load build-online.lisp "$@"

offline:
	sbcl --load build-offline.lisp "$@"

clean:
	rm -f online
	rm -f punter
	rm -f icfp-54733fce-c896-4892-a723-460fa5c85ece.tar.gz

package: clean offline
	tar -czvf icfp-54733fce-c896-4892-a723-460fa5c85ece.tar.gz PACKAGES install punter README init.lisp src/
