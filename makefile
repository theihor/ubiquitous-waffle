all:
	sbcl --load build.lisp "$@"
clean:
	rm -f main result.txt

test: all
	time ./main
