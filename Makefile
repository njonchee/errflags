VERSION=218

_ZIP=.zip

DISTBASE=errfl$(VERSION)
DISTFILE=$(DISTBASE)$(_ZIP)

all:	errflags.pas
	fpc $^
install:	errflags
	cp $^ /usr/local/bin/
clean:	
	rm -f errflags.o errflags
dist:	
	zip $(DISTFILE) errflags.cmt errflags.ctl errflags.exe errflags.pas errflags.tab errflags.txt FILE_ID.DIZ license.txt Makefile
hatch:	
	htick hatch $(DISTFILE) ERRFLAGS desc @DIZ
