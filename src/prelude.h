/* --------------------------------------------------------------------------
 * prelude.h:   Copyright (c) Mark P Jones 1991-1996.   All rights reserved.
 *              See NOTICE for details and conditions of use etc...
 *              Hugs version 1.3, August 1996
 *
 * Basic data type definitions, prototypes and standard macros including
 * machine dependent variations...
 * ------------------------------------------------------------------------*/

#define const		  /* const is more trouble than it's worth,...	   */
#include <stdio.h>

/*---------------------------------------------------------------------------
 * To select a particular machine/compiler, just place a 1 in the appropriate
 * position in the following list and ensure that 0 appears in all other
 * positions:
 *
 * The letters UN in the comment field indicate that I have not personally
 * been able to test this configuration yet and I have not heard from anybody
 * else that has tried it.  If you run Hugs on one of these systems and it
 * works (or needs patches) please let me know so that I can fix it and
 * update the source.
 *-------------------------------------------------------------------------*/

#define SOLARIS  1	/* For Solaris 2.4				   */
#define SUNOS    0      /* For Sun 3/Sun 4 running SunOs 4.x		   */
#define DJGPP2   0	/* For DJGPP version 2				   */
#define VC32	 0	/* For Microsoft Visual C++ 2.0 upwards		   */
#define BCC	 0	/* For IBM PC, using Borland C++ 3.1		UN */
#define OS2      0	/* For IBM OS/2 2.0 using EMX GCC		UN */
#define MIPS	 0	/* For MIPS RC6280/Sony machine NWS-3870	UN */
#define NEXTSTEP 0      /* For NeXTstep 3.0 using NeXT cc		UN */
#define NEXTGCC  0	/* For NeXTstep with gcc 2.x, doesn't work w/ NS3.2*/
#define MINIX68K 0	/* For Minix68k with gcc			UN */
#define AMIGA    0	/* For Amiga using gcc 2.2.2			UN */
#define HPUX     0      /* For HPUX using gcc				UN */
#define LINUX    0      /* For Linux using gcc				UN */
#define RISCOS   0	/* For Acorn DesktopC and RISCOS2 or 3		UN */
#define SVR4	 0	/* For SVR4 using GCC2.2			UN */
#define ULTRIX   0      /* For DEC Ultrix 4.x using GCC2.3.3		UN */
#define AIX	 0	/* For IBM AIX on RS/6000 using GCC		UN */
#define ATARI	 0	/* For Atari ST/STE/TT/Falcon w/ Lattice C 5.52 UN */
#define SGI4	 0	/* For SiliconGraphics Indigo, IRIX v*4*.0.5	UN */
#define NETBSD	 0	/* For NetBSD-current				UN */

/*---------------------------------------------------------------------------
 * To add a new machine/compiler, add a new macro line above, add the new
 * to the appropriate flags below and add a `machine specific' section in the
 * following section of this file.  Please send me details of any new machines
 * or compilers that you try so that I can pass them onto others!
 *
 *   UNIX           if the machine runs fairly standard Unix.
 *   SMALL_HUGS     for 16 bit operation on a limited memory PC.
 *   REGULAR_HUGS   for 32 bit operation using largish default table sizes.
 *   LARGE_HUGS     for 32 bit operation using larger default table sizes.
 *   JMPBUF_ARRAY   if jmpbufs can be treated like arrays.
 *   DOS_IO         to use DOS style IO for terminal control.
 *   TERMIO_IO      to use Unix termio for terminal control.
 *   SGTTY_IO       to use Unix sgtty for terminal control.
 *   TERMIOS_IO	    to use posix termios for terminal control.
 *   BREAK_FLOATS   to use two integers to store a float (or double)
 *		    if SMALL_HUGS, then you *must* use BREAK_FLOATS == 1
 *		    (assumes sizeof(int)==2, sizeof(float)==4).
 *		    Otherwise, assuming sizeof(int)==sizeof(float)==4,
 *                  BREAK_FLOATS == 0 will give you floats  for floating pt,
 *		    BREAK_FLOATS == 1 will give you doubles for floating pt.
 *   HAS_FLOATS	    to indicate support for floating point.
 *   HASKELL_ARRAYS to include support for Haskell array primitives.
 *   FLAT_ARRAYS    to use a flat array representation, if possible.
 *   IO_MONAD	    to include the IO monad primitives and support.
 *   IO_REFS	    Ref type for IO_MONAD, and simple operations.
 *   FLUSHEVERY	    to force a fflush after every char in putStr/hPutStr.
 *   LAZY_ST	    to include support for lazy state threads.
 *   NPLUSK	    to include support for (n+k) patterns.
 *   BIGNUMS	    to include support for Integer bignums.
 *   FIXED_SUBST    to force a fixed size for the current substitution.
 *   DYN_COMPS	    to allocate tables dynamically, currently just a memory
 *		    saving trick, but this may be extended at a later stage
 *		    to allow at least some of the tables to be extended
 *		    dynamically at run-time to avoid exhausted space errors.
 *   PROFILING	    to include support for profiling; WARNING: this can have
 *		    a significant, adverse effect on runtime performance.
 *-------------------------------------------------------------------------*/

#define UNIX		(SUNOS  | NEXTSTEP | HPUX | NEXTGCC | LINUX | AMIGA | \
			 MINIX68K | OS2 | SVR4 | ULTRIX | AIX | MIPS |\
			 SGI4 | NETBSD | SOLARIS)
#define SMALL_HUGS	(BCC)
#define REGULAR_HUGS	(RISCOS | ATARI)
#define LARGE_HUGS	(UNIX | DJGPP2 | VC32)
#define JMPBUF_ARRAY	(UNIX   | DJGPP2 | RISCOS | ATARI)
#define DOS_IO		(DJGPP2 | ATARI | BCC | VC32)
#define TERMIO_IO	(LINUX  | HPUX | OS2 | SVR4 | SGI4)
#define SGTTY_IO	(SUNOS  | NEXTSTEP | NEXTGCC | AMIGA | MINIX68K | \
			 ULTRIX | AIX | MIPS)
#define TERMIOS_IO      (NETBSD | SOLARIS)
#define BREAK_FLOATS	(BCC)
#define HAS_FLOATS	(REGULAR_HUGS | LARGE_HUGS | BREAK_FLOATS)

#define HASKELL_ARRAYS	1
#define FLAT_ARRAYS	0 /* Warning: Setting 1 is not currently supported */
#define IO_MONAD	1
#define IO_REFS		1 /* Experimental IO Ref type			   */
#define FLUSHEVERY	(DJGPP2)
#define LAZY_ST		(IO_MONAD)
#define NPLUSK		1 /* Warning: There are those that would prefer 0  */
#define BIGNUMS		1 /* Experimental bignum implementation		   */
#define FIXED_SUBST	0 /* Warning: This may not be appropriate for PCs  */
#define DYN_TABLES	0 /* For dynamically allocated tables	           */
#define PROFILING	0 /* Simplistic, incomplete, producer heap profiler*/

/*---------------------------------------------------------------------------
 * The following flags should be set automatically according to builtin
 * compiler flags, but you might want to set them manually to avoid default
 * behaviour in some situations:
 *-------------------------------------------------------------------------*/

#ifdef  __GNUC__			/* look for GCC 2.x extensions	   */
#if     __GNUC__ >= 2 && !NEXTSTEP	/* NeXT cc lies and says it's 2.x  */
#define GCC_THREADED 1

/* WARNING: if you use the following optimisations to assign registers for
 * particular global variables, you should be very careful to make sure that
 * storage(RESET) is called after a longjump (usually resulting from an error
 * condition) and before you try to access the heap.  The current version of
 * main deals with this using everybody(RESET) at the head of the main read,
 * eval, print loop
 */

#ifdef  m68k				/* global registers on an m68k	   */
#define GLOBALfst	asm("a4")
#define GLOBALsnd	asm("a5")
#define GLOBALsp	asm("a3")
#endif

#ifdef  sparc				/* global registers on a sparc	   */
/* sadly, although the gcc documentation suggests that the following reg   */
/* assignments should be ok, experience shows (at least on Suns) that they */
/* are not -- it seems that atof() and friends spoil things.		   */
/*#define GLOBALfst	asm("g5")*/
/*#define GLOBALsnd	asm("g6")*/
/*#define GLOBALsp	asm("g7")*/
#endif

#endif
#endif

#ifndef GCC_THREADED
#define GCC_THREADED 0
#endif

/*---------------------------------------------------------------------------
 * Machine specific sections:
 * Include any machine specific declarations and define macros:
 *   local              prefix for locally defined functions
 *   far                prefix for far pointers
 *   allowBreak()       call to allow user to interrupt computation
 *   FOPEN_WRITE        fopen *text* file for writing
 *   FOPEN_APPEND       fopen *text* file for append
 *   FOPEN_READ         fopen *text* file for read
 *
 * N.B. `far' must be explicitly defined (usually to the empty string)
 *-------------------------------------------------------------------------*/

#ifdef __STDC__           /* To enable use of prototypes whenever possible */
#define Args(x) x
#else
#if	(BCC)		  /* K&R 1 does not permit `defined(__STDC__)' ... */
#define Args(x) x
#else
#define Args(x) ()
#endif
#endif

#if     (SUNOS | SOLARIS)
#include <malloc.h>
#define far
#define farCalloc(n,s)	(Void *)valloc(((unsigned)n)*((unsigned)s))
#endif

#if	VC32
#include <malloc.h>
#include <io.h>
#define local
#define far
#define ctrlbrk(bh)	signal(SIGINT,bh); signal(SIGBREAK,bh)
#define allowBreak()	if (broken) { broken=FALSE; sigRaise(breakHandler); }
extern void ignoreBreak(int);
#endif

#if     (BCC)
#include <alloc.h>
#define local		near pascal
extern  int  kbhit	Args((void));
#define allowBreak()	kbhit()
#define FOPEN_WRITE	"wt"
#define FOPEN_APPEND	"at"
#define FOPEN_READ	"rt"
#define farCalloc(n,s)	farcalloc((unsigned long)n,(unsigned long)s)
#define sigProto(nm)	int nm(void)
#define sigRaise(nm)	nm()
#define sigHandler(nm)	int nm()
#define sigResume	return 1
#if (BCC)
extern  int    stricmp	Args((char *, char*));
#endif
#define strCompare	stricmp
#endif

#if     MIPS
#define far
#define farCalloc(n,s)	(Void *)valloc(((unsigned)n)*((unsigned)s))
#endif

#if     (NEXTSTEP | NEXTGCC | MINIX68K | ULTRIX)
#include <stdlib.h>
#define far
#define farCalloc(n,s)	(Void *)valloc(((unsigned)n)*((unsigned)s))
#endif

#if     AMIGA
#include <stdlib.h>
#define	Main		int
#define	far
#define	farCalloc(n,s)	(Void *)valloc(((unsigned)n)*((unsigned)s))
#endif

#if     (HPUX | DJGPP2 | LINUX | OS2 | SVR4 | AIX | SGI4 | NETBSD)
#include <stdlib.h>
#define  far
#endif

#if	RISCOS
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#define  far
#define  isascii(c)	(((unsigned)(c))<128)
#define  Main		int
#define  MainDone	return 0;/*NOTUSED*/
extern   int access	Args((char *, int));
extern   int namecmp    Args((char *, char *));
#endif

#ifndef USE_READLINE
#define USE_READLINE  0
#endif
#ifndef allowBreak
#define allowBreak()
#endif
#ifndef local
#define local
#endif
#ifndef farCalloc
#define farCalloc(n,s)	   (Void *)calloc(((unsigned)n),((unsigned)s))
#endif
#ifndef FOPEN_WRITE
#define FOPEN_WRITE	   "w"
#endif
#ifndef FOPEN_APPEND
#define FOPEN_APPEND	   "a"
#endif
#ifndef FOPEN_READ
#define FOPEN_READ	   "r"
#endif
#ifndef sigProto
#define sigProto(nm)	   Void nm Args((int))
#define sigRaise(nm)	   nm(1)
#define sigHandler(nm)	   Void nm(sig_arg) int sig_arg;
#define sigResume	   return
#endif
#ifndef Main			/* to cope with systems that don't like	   */
#define Main		   Void /* main to be declared as returning Void   */
#endif
#ifndef MainDone
#define MainDone
#endif
#ifndef strCompare
#define strCompare	   strcmp
#endif

#if (UNIX | DJGPP2 | RISCOS | ATARI)
#define ctrlbrk(bh)	   signal(SIGINT,bh)
#endif

/*---------------------------------------------------------------------------
 * General settings:
 *-------------------------------------------------------------------------*/

#define Void     void   /* older compilers object to: typedef void Void;   */
typedef unsigned Bool;
#define TRUE     1
#define FALSE    0
typedef char    *String;
typedef int      Int;
typedef long     Long;
typedef int      Char;
typedef unsigned Unsigned;

#ifndef STD_PRELUDE
#if     RISCOS
#define STD_PRELUDE	   "prelude"
#else
#define STD_PRELUDE	   "Prelude.hs"
#endif
#endif

#define NUM_SYNTAX         100
#define NUM_SELECTS        100
#if IO_MONAD
#define NUM_HANDLES	   20
#endif
#define NUM_SCRIPTS        64
#define NUM_FIXUPS         100
#define NUM_TUPLES         100
#define NUM_OFFSETS        1024
#define NUM_CHARS          256

#if PROFILING
#define DEF_PROFINTDIV	   10		/* hpsize/this cells between samples*/
#endif

#if     SMALL_HUGS			/* the McDonalds mentality :-)	   */
#define Pick(s,r,l)	   s
#endif
#if     REGULAR_HUGS
#define Pick(s,r,l)	   r
#endif
#if     LARGE_HUGS
#define Pick(s,r,l)	   l
#endif

#define NUM_TYCON          Pick(60,    160,        160)
#define NUM_NAME           Pick(1000,  2000,       16000)
#define NUM_CLASSES        Pick(30,    40,         40)
#define NUM_INSTS          Pick(200,   300,        400)
#define NUM_DICTS          Pick(1000,  10000,      32000)
#define NUM_FLAT	   Pick(1000,  10000,	   32000)
#define NUM_TEXT           Pick(12000, 20000,      80000)
#define NUM_TEXTH	   Pick(1,     10,         10)
#define NUM_TYVARS         Pick(800,   2000,       4000)
#define NUM_STACK          Pick(1800,  12000,      16000)
#define NUM_ADDRS          Pick(28000, 60000,      320000)
#define MINIMUMHEAP	   Pick(7500,  7500,       7500)
#define MAXIMUMHEAP	   Pick(32765, 0,          0)
#define DEFAULTHEAP        Pick(28000, 50000,      100000)
#define MAXPOSINT          Pick(32767, 2147483647, 2147483647)
#define NUM_DTUPLES	   Pick(3,     5,          5)
#define BIGBASE		   Pick(100,   10000,      10000)
#define BIGEXP		   Pick(2,     4,          4)

#if DYN_TABLES				/* Tables may be alloc'd at runtime*/
#define DECTABLE(tab)	   far *tab	/* macros for declaration & defn   */
#define DEFTABLE(tab,sz)   far *tab = 0
#else					/* or at compile-time:		   */
#define DECTABLE(tab)	   tab[]
#define DEFTABLE(tab,sz)   tab[sz]
#endif

#define minRecovery	   Pick(1000,  1000,       1000)
#define bitsPerWord	   Pick(16,    32,         32)
#define wordShift	   Pick(4,     5,          5)
#define wordMask	   Pick(15,    31,         31)

#define bitArraySize(n)    ((n)/bitsPerWord + 1)
#define placeInSet(n)      ((-(n)-1)>>wordShift)
#define maskInSet(n)       (1<<((-(n)-1)&wordMask))

#ifndef __GNUC__
#if !RISCOS && !VC32
extern Int      strcmp     Args((String, String));
extern Int      strlen     Args((String));
extern char	*strcpy	   Args((String,String));
extern char     *strcat	   Args((String,String));
#endif
#endif
#if !LINUX
extern char	*getenv	   Args((char *));
extern int      system	   Args((const char *));
extern int 	chdir 	   Args((String));
extern double   atof	   Args((char *));
#endif
extern char     *strchr    Args((char *,int));  /* test membership in str  */
#if !DJGPP2
extern Void     exit       Args((Int));
#endif
extern Void     internal   Args((String));
extern Void     fatal	   Args((String));

#if     HAS_FLOATS

#if	(REGULAR_HUGS | LARGE_HUGS) & BREAK_FLOATS
#define FloatImpType	   double
#define FloatPro	   double
#define FloatFMT           "%.9g"
#else
#define FloatImpType	   float
#define FloatPro	   double  /* type to use in prototypes		   */
				   /* strictly ansi (i.e. gcc) conforming  */
				   /* but breaks data hiding :-(	   */
#define FloatFMT	   "%g"
#endif
#else
#define FloatImpType	   int     /*dummy*/
#define FloatPro	   int
#define FloatFMT	   "%d"
#endif

#ifndef FILENAME_MAX	   /* should already be defined in an ANSI compiler*/
#define FILENAME_MAX 256
#else
#if     FILENAME_MAX < 256
#undef  FILENAME_MAX
#define FILENAME_MAX 256
#endif
#endif

/*-------------------------------------------------------------------------*/
