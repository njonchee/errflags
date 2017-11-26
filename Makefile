all:	errflags.pas
	fpc $^
install:	errflags
	cp $^ /usr/local/bin/
clean:	
	rm -f errflags.o errflags
dist:	
	zip errfl218.zip errflags.cmt errflags.ctl errflags.exe errflags.pas errflags.tab errflags.txt FILE_ID.DIZ license.txt Makefile
