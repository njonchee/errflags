all:	errflags.pas
	fpc $^
install:	errflags
	cp $^ /usr/local/bin/
clean:	
	rm -f errflags.o errflags
