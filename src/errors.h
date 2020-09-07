/* --------------------------------------------------------------------------
 * errors.h:    Copyright (c) Mark P Jones 1991-1996.   All rights reserved.
 *              See NOTICE for details and conditions of use etc...
 *              Hugs version 1.3, August 1996
 *
 * Error handling support functions
 * ------------------------------------------------------------------------*/

#define errorStream	 stdout
#define Hilite
#define Lolite

#define ERRMSG(l)	 Hilite errHead(l);fprintf(errorStream,
#define EEND       	 ); Lolite errFail()
#define ETHEN		 );
#define ERRTEXT		 Hilite fprintf(errorStream,
#define ERREXPR(e)	 Hilite printExp(errorStream,e); Lolite
#define ERRTYPE(e)	 Hilite printType(errorStream,e); Lolite
#define ERRCONTEXT(qs)   Hilite printContext(errorStream,qs); Lolite
#define ERRPRED(pi)      Hilite printPred(errorStream,pi); Lolite
#define ERRKIND(k)	 Hilite printKind(errorStream,k); Lolite

extern Void errHead      Args((Int));              /* in main.c            */
extern Void errFail      Args((Void));
extern Void errAbort	 Args((Void));

extern sigProto(breakHandler);

extern Bool breakOn      Args((Bool));		   /* in machdep.c	   */

extern Void printExp     Args((FILE *,Cell));      /* in output.c          */
extern Void printType    Args((FILE *,Cell));
extern Void printContext Args((FILE *,List));
extern Void printPred    Args((FILE *,Cell));
extern Void printKind	 Args((FILE *,Kind));

/*-------------------------------------------------------------------------*/
