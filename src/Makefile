# --------------------------------------------------------------------------
# Makefile:	Copyright (c) Mark P Jones 1991-1996.   All rights reserved.
#		See NOTICE for details and conditions of use etc...
#		Hugs version 1.3, August 1996
#
# Standard makefile for DOS (djgpp) and Unix (Solaris, etc.)
# --------------------------------------------------------------------------

.SUFFIXES	: .y .c .h .o .gs .s
HFILES		= prelude.h storage.h connect.h errors.h command.h
CFILES		= hugs.c storage.c input.c static.c type.c \
		  output.c compiler.c machine.c builtin.c
INCFILES	= parser.c preds.c bignums.c kind.c subst.c \
		  machdep.c printer.c iomonad.c array.c
YFILES		= parser.y
SOURCES		= $(HFILES) $(CFILES) $(INCFILES) $(YFILES) prelude
OBJECTS		= storage.o input.o static.o type.o compiler.o
IOBJECTS	= hugs.o builtin.o  machine.o output.o $(OBJECTS)

# Edit the following settings as required.
# There are two choices of command line editor that can be used with Hugs:
#
#  GNU readline:		usual GNU sources (e.g. bash distribution)
#  add -DUSE_READLINE=1 to CFLAGS and libreadline.a -ltermcap to LDFLAGS
#				      (or maybe -lreadline -ltermcap)
#
#  editline:			(comp.sources.misc, vol 31, issue 71)
#  add -DUSE_READLINE=1 to CFLAGS and libedit.a to LDFLAGS
#				      (or maybe -ledit)
#
# The best bet is to `touch prelude.h' after changing these settings to
# ensure that the whole collection of files is recompiled with the correct
# settings.

CC		= gcc
CFLAGS		= 
LDFLAGS    	= -lm
OPTFLAGS	= -O

all		: hugs

hugs		: $(IOBJECTS)
		  $(CC) $(OPTFLAGS) $(IOBJECTS) -o hugs $(LDFLAGS)
		  strip hugs
#		  coff2exe hugs		# for DJGPP2

.c.o		:
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) $<

clean		:
		  rm *.o

parser.c	: parser.y
		  -yacc parser.y
		  mv y.tab.c parser.c

hugs.o		: prelude.h storage.h connect.h errors.h \
		  command.h machdep.c
storage.o	: prelude.h storage.h connect.h errors.h
input.o		: prelude.h storage.h connect.h errors.h parser.c command.h
static.o	: prelude.h storage.h connect.h errors.h
type.o		: prelude.h storage.h connect.h errors.h preds.c kind.c subst.c
output.o	: prelude.h storage.h connect.h errors.h
compiler.o	: prelude.h storage.h connect.h errors.h
		  $(CC) -c $(CFLAGS) compiler.c
machine.o	: prelude.h storage.h connect.h errors.h
builtin.o	: prelude.h storage.h connect.h errors.h \
		  bignums.c printer.c iomonad.c array.c

