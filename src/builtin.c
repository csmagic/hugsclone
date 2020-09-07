/* --------------------------------------------------------------------------
 * builtin.c:   Copyright (c) Mark P Jones 1991-1996.   All rights reserved.
 *              See NOTICE for details and conditions of use etc...
 *              Hugs version 1.3, August 1996
 *
 * Primitive functions, input output etc...
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include <ctype.h>
#include <math.h>
#include <float.h>
#if 	 (BCC)
#include <io.h>
#endif

Name nameNegate,  nameFlip;		/* primitives reqd for parsing	   */
Name nameFrom,    nameFromThen;
Name nameFromTo,  nameFromThenTo;
Name nameFatbar,  nameFail;		/* primitives reqd for translation */
Name nameIf,	  nameSel;
Name nameFst,	  nameSnd;		/* standard combinators		   */
Name nameId,	  nameOtherwise;
Name nameConCmp,  nameEnRange;		/* primitives used for deriv inst  */
Name nameEnIndex, nameEnInRng;
Name nameEnToEn,  nameEnFrEn;
Name nameEnFrom,  nameEnFrTh;
Name nameEnFrTo;
Name nameUndefMem;			/* undefined member primitive	   */
Name nameMakeMem;			/* makeMember primitive		   */
Name nameBlackHole;			/* for GC-detected black hole	   */
Name namePrint,   nameNPrint;		/* primitives for printing	   */
Name nameIStrict, nameISeq;		/* primitives for strictness	   */

Name nameAnd,     nameOr;		/* built-in logical connectives	   */
Name nameError;				/* error primitive function	   */
Name nameUndefined;			/* generic undefined value	   */
Name nameComp;				/* function composition		   */
Name nameApp;				/* list append			   */
Name nameShowField;			/* display single field		   */
Name nameShowParen;			/* wrap with parens		   */
Name nameRangeSize;			/* calculate size of index range   */
Name nameCompAux;			/* auxiliary function for compares */
Name namePmInt,   namePmFlt;		/* primitives for pattern matching */
Name namePmInteger;
#if NPLUSK
Name namePmNpk,   namePmSub;		/* primitives for (n+k) patterns   */
#endif

#if    HASKELL_ARRAYS
static Name nameEltUndef;		/* undefined element in array	   */
static Name nameOutBounds;		/* value of of bounds		   */
#endif
#if    IO_MONAD
static Name nameUncaught, nameDone;	/* top-level continuations	   */
static Name namePass;			/* auxiliary:: \f b c a -> f a b c */
#endif
#if    LAZY_ST
Name   nameSTRun;			/* encapsulation operator for IO   */
#endif

static FILE *writingFile = 0;		/* points to file open for writing */

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

#define PROTO_PRIM(name)	static Void name Args((StackPtr))
#define primFun(name)		static Void name(root) StackPtr root;
#define primArg(n)		stack(root+n)

/* IMPORTANT: the second element of an update must be written first.
 * this is to deal with the case where an INDIRECT tag is written into
 * a Cell before the second value has been set.  If a garbage collection
 * occurs before the second element was set then the INDIRECTion will be
 * (wrongly) elided and result in chaos.  I know.  It happened to me.
 */

#define update(l,r)		((snd(stack(root))=r),(fst(stack(root))=l))
#define updateRoot(c)		update(INDIRECT,c)
#define updapRoot(l,r)		update(l,r)
#define cantReduce()		evalFails(root)

PROTO_PRIM(primFatbar);
PROTO_PRIM(primFail);
PROTO_PRIM(primSel);
PROTO_PRIM(primIf);
PROTO_PRIM(primStrict);
PROTO_PRIM(primSeq);
PROTO_PRIM(primTrace);
PROTO_PRIM(primMakeMem);
PROTO_PRIM(primConCmp);
PROTO_PRIM(primEnRange);
PROTO_PRIM(primEnIndex);
PROTO_PRIM(primEnInRng);
PROTO_PRIM(primEnFrEn);
PROTO_PRIM(primEnToEn);
PROTO_PRIM(primEnFrom);
PROTO_PRIM(primEnFrTh);
PROTO_PRIM(primEnFrTo);

#if HASKELL_ARRAYS
PROTO_PRIM(primArray);
PROTO_PRIM(primUpdate);
PROTO_PRIM(primAccum);
PROTO_PRIM(primAccumArray);
PROTO_PRIM(primAmap);
PROTO_PRIM(primSubscript);
PROTO_PRIM(primBounds);
PROTO_PRIM(primElems);
#endif

PROTO_PRIM(primMinInt);
PROTO_PRIM(primMaxInt);
PROTO_PRIM(primPlusInt);
PROTO_PRIM(primMinusInt);
PROTO_PRIM(primMulInt);
PROTO_PRIM(primDivInt);
PROTO_PRIM(primQuotInt);
PROTO_PRIM(primModInt);
PROTO_PRIM(primRemInt);
PROTO_PRIM(primQrmInt);
PROTO_PRIM(primNegInt);
PROTO_PRIM(primEvenInt);

#if BIGNUMS
PROTO_PRIM(primPlusInteger);
PROTO_PRIM(primMinusInteger);
PROTO_PRIM(primMulInteger);
PROTO_PRIM(primQrmInteger);
PROTO_PRIM(primNegInteger);
PROTO_PRIM(primEvenInteger);
PROTO_PRIM(primIntToInteger);
PROTO_PRIM(primIntegerToInt);
PROTO_PRIM(primIntegerToFloat);
PROTO_PRIM(primEqInteger);
PROTO_PRIM(primCmpInteger);
#endif

PROTO_PRIM(primCharToInt);
PROTO_PRIM(primIntToChar);
PROTO_PRIM(primIntToFloat);
PROTO_PRIM(primDummyCvt);

PROTO_PRIM(primPlusFloat);
PROTO_PRIM(primMinusFloat);
PROTO_PRIM(primMulFloat);
PROTO_PRIM(primDivFloat);
PROTO_PRIM(primNegFloat);

#if HAS_FLOATS
PROTO_PRIM(primPiFloat);
PROTO_PRIM(primSinFloat);
PROTO_PRIM(primCosFloat);
PROTO_PRIM(primTanFloat);
PROTO_PRIM(primAsinFloat);
PROTO_PRIM(primAcosFloat);
PROTO_PRIM(primAtanFloat);
PROTO_PRIM(primExpFloat);
PROTO_PRIM(primLogFloat);
PROTO_PRIM(primSqrtFloat);
PROTO_PRIM(primFloatToInt);
PROTO_PRIM(primFloatRadix);
PROTO_PRIM(primFloatDigits);
PROTO_PRIM(primFloatRange);
PROTO_PRIM(primFloatDecode);
PROTO_PRIM(primFloatEncode);
#endif

PROTO_PRIM(primEqInt);
PROTO_PRIM(primCmpInt);

PROTO_PRIM(primEqChar);
PROTO_PRIM(primCmpChar);

PROTO_PRIM(primEqFloat);
PROTO_PRIM(primCmpFloat);

PROTO_PRIM(primPrint);
PROTO_PRIM(primBPrint);
PROTO_PRIM(primNPrint);
PROTO_PRIM(primLPrint);
PROTO_PRIM(primNLPrint);
PROTO_PRIM(primSPrint);
PROTO_PRIM(primNSPrint);

static String local evalName		Args((Cell));

#if IO_MONAD
PROTO_PRIM(primUncaught);
PROTO_PRIM(primDone);
PROTO_PRIM(primLunit);
PROTO_PRIM(primRunit);
PROTO_PRIM(primLbind);
PROTO_PRIM(primRbind);
PROTO_PRIM(primPass);
PROTO_PRIM(primGetCh);
PROTO_PRIM(primGetChar);
PROTO_PRIM(primHGetChar);
PROTO_PRIM(primPutChar);
PROTO_PRIM(primHPutChar);
PROTO_PRIM(primPutStr);
PROTO_PRIM(primHPutStr);
PROTO_PRIM(primHreader);
PROTO_PRIM(primHContents);
PROTO_PRIM(primContents);
PROTO_PRIM(primOpenFile);
PROTO_PRIM(primStdin);
PROTO_PRIM(primStdout);
PROTO_PRIM(primStderr);
PROTO_PRIM(primHIsEOF);
PROTO_PRIM(primHFlush);
PROTO_PRIM(primHClose);
PROTO_PRIM(primReadFile);
PROTO_PRIM(primWriteFile);
PROTO_PRIM(primAppendFile);

static Void local fwritePrim  Args((StackPtr,Bool));

PROTO_PRIM(primUserError);
PROTO_PRIM(primIsUserErr);
PROTO_PRIM(primIsIllegal);
PROTO_PRIM(primIsUnsupported);
PROTO_PRIM(primGetHandle);
PROTO_PRIM(primGetFileName);

#if IO_REFS
PROTO_PRIM(primNewRef);
PROTO_PRIM(primDerefRef);
PROTO_PRIM(primAssignRef);
PROTO_PRIM(primEqRef);
#endif
#endif

#if LAZY_ST
PROTO_PRIM(primSTRun);
PROTO_PRIM(primSTReturn);
PROTO_PRIM(primSTBind);
PROTO_PRIM(primSTInter);
PROTO_PRIM(primSTNew);
PROTO_PRIM(primSTAssign);
PROTO_PRIM(primSTDeref);
PROTO_PRIM(primSTMutVarEq);
#if HASKELL_ARRAYS
PROTO_PRIM(primSTNewArr);
PROTO_PRIM(primSTReadArr);
PROTO_PRIM(primSTWriteArr);
PROTO_PRIM(primSTFreeze);
#endif
#endif

/* --------------------------------------------------------------------------
 * Table of primitive/built-in values:
 * ------------------------------------------------------------------------*/

struct primitive primitives[] = {
  {"fatbar",		2, primFatbar},
  {"fail",		0, primFail},
  {"undefMem",		1, primFail},
  {"gcBhole",		0, primFail},
  {"error",		1, primFail},
  {"sel",		3, primSel},
  {"if",		3, primIf},
  {"trace",		2, primTrace},
  {"makeMem",		2, primMakeMem},
  {"conCmp",		2, primConCmp},
  {"enRange",		1, primEnRange},
  {"enIndex",		2, primEnIndex},
  {"enInRng",		2, primEnInRng},
  {"enToEn",		2, primEnToEn},
  {"enFrEn",		1, primEnFrEn},
  {"enFrom",		1, primEnFrom},
  {"enFrTh",		2, primEnFrTh},
  {"enFrTo",		2, primEnFrTo},

#if HASKELL_ARRAYS
  {"primArray",		3, primArray},
  {"primUpdate",	3, primUpdate},
  {"primAccum",		4, primAccum},
  {"primAccumArray",	5, primAccumArray},
  {"primAmap",		2, primAmap},
  {"primSubscript",	3, primSubscript},
  {"primBounds",	1, primBounds},
  {"primElems",		1, primElems},
  {"eltUndef",		0, primFail},
  {"outBounds",		2, primFail},
#endif

  {"print",		3, primPrint},
  {"nprint",		3, primNPrint},
  {"lprint",		2, primLPrint},
  {"nlprint",		2, primNLPrint},
  {"sprint",		2, primSPrint},
  {"nsprint",		2, primNSPrint},

  {"primMinInt",	0, primMinInt},
  {"primMaxInt",	0, primMaxInt},
  {"primPlusInt",	2, primPlusInt},
  {"primMinusInt",	2, primMinusInt},
  {"primMulInt",	2, primMulInt},
  {"primDivInt",	2, primDivInt},
  {"primQuotInt",	2, primQuotInt},
  {"primModInt",	2, primModInt},
  {"primRemInt",	2, primRemInt},
  {"primNegInt",	1, primNegInt},
  {"primEvenInt",	1, primEvenInt},
  {"primQrmInt",	2, primQrmInt},

#if BIGNUMS				/* Bignum primitive functions	   */
  {"primPlusInteger",	2, primPlusInteger},
  {"primMinusInteger",	2, primMinusInteger},
  {"primMulInteger",	2, primMulInteger},
  {"primQrmInteger",	2, primQrmInteger},
  {"primNegInteger",	1, primNegInteger},
  {"primEvenInteger",	1, primEvenInteger},
  {"primIntToInteger",	1, primIntToInteger},
  {"primIntegerToInt",	1, primIntegerToInt},
  {"primIntegerToFloat",1, primIntegerToFloat},
  {"primIntegerToDouble",1,primIntegerToFloat},
  {"primEqInteger",	2, primEqInteger},
  {"primCmpInteger",	2, primCmpInteger},
#else					/* Implement Integer as Int	   */
  {"primPlusInteger",	2, primPlusInt},
  {"primMinusInteger",	2, primMinusInt},
  {"primMulInteger",	2, primMulInt},
  {"primQrmInteger",	2, primQrmInt},
  {"primNegInteger",	1, primNegInt},
  {"primIntToInteger",  1, primDummyCvt},
  {"primIntegerToInt",  1, primDummyCvt},
  {"primIntegerToFloat",1, primIntToFloat},
  {"primIntegerToDouble",1,primIntToFloat},
  {"primEqInteger",	2, primEqInt},
  {"primCmpInteger",	2, primCmpInt},
#endif

  {"primPlusFloat",	2, primPlusFloat},
  {"primMinusFloat",	2, primMinusFloat},
  {"primMulFloat",	2, primMulFloat},
  {"primDivFloat",	2, primDivFloat},
  {"primNegFloat",	1, primNegFloat},

  {"primPlusDouble",	2, primPlusFloat},	/* Currently Float */
  {"primMinusDouble",	2, primMinusFloat},	/* Currently Float */
  {"primMulDouble",	2, primMulFloat},	/* Currently Float */
  {"primDivDouble",	2, primDivFloat},	/* Currently Float */
  {"primNegDouble",	1, primNegFloat},	/* Currently Float */

#if HAS_FLOATS
  {"primPiFloat",	0, primPiFloat},
  {"primSinFloat",	1, primSinFloat},
  {"primCosFloat",	1, primCosFloat},
  {"primTanFloat",	1, primTanFloat},
  {"primAsinFloat",	1, primAsinFloat},
  {"primAcosFloat",	1, primAcosFloat},
  {"primAtanFloat",	1, primAtanFloat},
  {"primExpFloat",	1, primExpFloat},
  {"primLogFloat",	1, primLogFloat},
  {"primSqrtFloat",	1, primSqrtFloat},
  {"primFloatToInt",	1, primFloatToInt},
  {"primFloatRadix",	1, primFloatRadix},
  {"primFloatDigits",	1, primFloatDigits},
  {"primFloatRange",	1, primFloatRange},
  {"primFloatDecode",	1, primFloatDecode},
  {"primFloatEncode",	2, primFloatEncode},

  {"primPiDouble",	0, primPiFloat},	/* Currently Float */
  {"primSinDouble",	1, primSinFloat},	/* Currently Float */
  {"primCosDouble",	1, primCosFloat},	/* Currently Float */
  {"primTanDouble",	1, primTanFloat},	/* Currently Float */
  {"primAsinDouble",	1, primAsinFloat},	/* Currently Float */
  {"primAcosDouble",	1, primAcosFloat},	/* Currently Float */
  {"primAtanDouble",	1, primAtanFloat},	/* Currently Float */
  {"primExpDouble",	1, primExpFloat},	/* Currently Float */
  {"primLogDouble",	1, primLogFloat},	/* Currently Float */
  {"primSqrtDouble",	1, primSqrtFloat},	/* Currently Float */
  {"primDoubleToInt",	1, primFloatToInt},	/* Currently Float */
  {"primDoubleRadix",	1, primFloatRadix},	/* Currently Float */
  {"primDoubleDigits",	1, primFloatDigits},	/* Currently Float */
  {"primDoubleRange",	1, primFloatRange},	/* Currently Float */
  {"primDoubleDecode",	1, primFloatDecode},	/* Currently Float */
  {"primDoubleEncode",	2, primFloatEncode},	/* Currently Float */
#endif

  {"primIntToChar",	1, primIntToChar},
  {"primCharToInt",	1, primCharToInt},
  {"primIntToFloat",	1, primIntToFloat},
  {"primIntToDouble",	1, primIntToFloat},	/* Currently Float */
  {"primDoubleToFloat", 1, primDummyCvt},	/* dummy	   */

  {"primEqInt",		2, primEqInt},
  {"primCmpInt",	2, primCmpInt},
  {"primEqChar",	2, primEqChar},
  {"primCmpChar",	2, primCmpChar},
  {"primEqFloat",	2, primEqFloat},
  {"primCmpFloat",	2, primCmpFloat},
  {"primEqDouble",	2, primEqFloat},	/* Currently Float */
  {"primCmpDouble",	2, primCmpFloat},	/* Currently Float */

  {"primShowsInt",	3, primBPrint},
  {"primShowsInteger",	3, primBPrint},
  {"primShowsFloat",	3, primBPrint},
  {"primShowsDouble",	3, primBPrint},

  {"strict",		2, primStrict},
  {"seq",		2, primSeq},

#if IO_MONAD
  {"uncaughtIO", 	1, primUncaught},
  {"doneIO",		1, primDone},
  {"lunitIO",		3, primLunit},
  {"runitIO",		3, primRunit},
  {"lbindIO",		4, primLbind},
  {"rbindIO",		4, primRbind},
  {"passIO",		4, primPass},
  {"getCh",		2, primGetCh},
  {"getChar",		2, primGetChar},
  {"hGetChar",		3, primHGetChar},
  {"putChar",		3, primPutChar},
  {"hPutChar",		4, primHPutChar},
  {"putStr",		3, primPutStr},
  {"hPutStr",		4, primHPutStr},
  {"hreader",		1, primHreader},
  {"hGetContents",	3, primHContents},
  {"getContents",	2, primContents},
  {"openFile",          4, primOpenFile},
  {"stdin",		0, primStdin},
  {"stdout",		0, primStdout},
  {"stderr",		0, primStderr},
  {"hIsEOF",		3, primHIsEOF},
  {"hFlush",		3, primHFlush},
  {"hClose",		3, primHClose},
  {"readFile",		3, primReadFile},
  {"writeFile",		4, primWriteFile},
  {"appendFile",	4, primAppendFile},

  {"userError",		1, primUserError},
  {"isUserError",	1, primIsUserErr},
  {"isIllegalError",	1, primIsIllegal},
  {"isAlreadyExists",	1, primIsUnsupported},
  {"isAlreadyInUse",	1, primIsUnsupported},
  {"isFullError",	1, primIsUnsupported},
  {"isEOFError",	1, primIsUnsupported},
  {"isPermissionError",	1, primIsUnsupported},
  {"ioeGetHandle",	1, primGetHandle},
  {"ioeGetFileName",	1, primGetFileName},
#if IO_REFS
  {"newRef",            3, primNewRef},
  {"getRef",		3, primDerefRef},
  {"setRef",		4, primAssignRef},
  {"eqRef",		2, primEqRef},
#endif
#endif

#if LAZY_ST
  {"STRun",		1, primSTRun},
  {"STReturn",		1, primSTReturn},
  {"STBind",		3, primSTBind},
  {"STInter",		2, primSTInter},
  {"STNew",		2, primSTNew},
  {"STAssign",		3, primSTAssign},
  {"STDeref",		2, primSTDeref},
  {"STMutVarEq",	2, primSTMutVarEq},
#if HASKELL_ARRAYS
  {"STNewArr",		4, primSTNewArr},
  {"STReadArr",		4, primSTReadArr},
  {"STWriteArr",	5, primSTWriteArr},
  {"STFreeze",		2, primSTFreeze},
#endif
#endif

  {0,			0, 0}
};

/* --------------------------------------------------------------------------
 * Primitive functions:
 *
 * IMPORTANT NOTICE: the primitive function definitions in this file
 * should be written in a style that permits correct execution *without*
 * conservative garbage collection (i.e., without marking from the C stack).
 * Adding primitive definitions that do not meet this requirement may
 * corrupt the heap and lead to failed execution; do not modify this code
 * unless you are really confident about what you are doing.
 *
 * Some general guidelines follow, using c, e to denote expressions that
 * involve either at most 1 allocation, or the possibility/certainty of
 * multiple allocations, resp.
 *
 * push(c);		Ok.
 * push(e);		Bad -- intermediate result may be lost if GC occurs
 *			in the middle of building e; break e into steps, and
 *			use toparg(), topfun(), etc.
 *
 * Cell x = ...;	Safe if value assigned to x will never be an
 * <any code with a	indirection.  (Otherwise, cell assigned to x may
 * possible alloc>	be returned to freeList *before* the value is used.)
 * ... x ...		Probably best avoided in other circumstances.
 *
 * updateRoot(e);	All ok.
 * updapRoot(e,e);
 * updateRoot(mkInt(n));
 * eval(pop());
 *
 * eval(ap(c,pop()));	Bad -- a GC call may corrupt value pop'd off stack.
 *
 * It is also worth a reminder that the fst and snd values passed in any call
 * to the allocator are automatically marked and preserved if a GC is needed.
 * As a result, code like the following is guaranteed to be safe:
 *  return ap(ap(mkTuple(2),ZERONUM),ZERONUM);    (ZERONUM is a constant)
 *  for ( ... )					  (PROVIDED that ds is the
 *     ds = cons(consChar(c),ds);		   only var that needs GC).
 *
 * If these restrictions are judged to be too onerous in particular cases,
 * temporarily enable conservative GC (and reset it to the original state,
 * either on or off at the beginning of the operation).  See primMakeMem
 * for an example.
 *
 * There are also certain conventions that must always be obeyed, regardless
 * of whether conservative GC is in use.  For example:
 *
 * lhs = expr;		If lhs involves an address calculation that may be
 * 			invalidated by a gc, and expr could trigger an alloc,
 *			then this expression is bad, or at least not portable:
 *			it will only do the right thing under some evaluation
 *			orders.  For example:  hd(top()) = ap(..,..) is bad,
 *			unless you know that top() will never be modified
 * 			during a GC.
 *
 *			This is no different from the problems that occur
 *			with non-portable combinations of stack operators
 *			like push(top());  The solution is also the same:
 *			use an intermediate variable to make the order
 *			of evaluation explicit.
 *
 * If this version of Hugs has been modified to allow different or
 * additional run-time representations for certain values, then the
 * examples and principles illustrated here may need to be reconsidered,
 * and possibly reclassified.  The same will also be true if the execution
 * mechanisms etc., are changed in any way.  (And all this is assuming
 * that the original implementations are correct...)
 * ------------------------------------------------------------------------*/

primFun(primFatbar) {			/* Fatbar primitive		   */
    Cell temp = evalWithNoError(primArg(2));
    if (nonNull(temp))
	if (temp==nameFail)		/* _FAIL [] r = r		   */
	    updateRoot(primArg(1));
	else {
	    updateRoot(temp);
	    cantReduce();
	}
    else
	updateRoot(primArg(2));		/* l     [] r = l  -- otherwise	   */
}

primFun(primFail) {			/* Failure primitive		   */
    cantReduce();
}

primFun(primSel) {			/* Component selection		   */
    eval(primArg(2));			/* _sel c e n  return nth component*/
    if (whnfHead==primArg(3))		/* in expr e, built with cfun c	   */
	updateRoot(pushed(intOf(primArg(1))-1));
    else
	cantReduce();
}

primFun(primIf) {			/* Conditional primitive 	   */
    eval(primArg(3));
    if (whnfHead==nameTrue)
	updateRoot(primArg(2));
    else
	updateRoot(primArg(1));
}

primFun(primStrict) {		       /* Strict application primitive	   */
    eval(primArg(1));		       /* evaluate 2nd argument 	   */
    updapRoot(primArg(2),primArg(1));  /* and apply 1st argument to result */
}

primFun(primSeq) {			/* Strict sequencing primitive	   */
    eval(primArg(2));			/* evaluate 1st argument	   */
    updateRoot(primArg(1));		/* and return the first		   */
}

primFun(primTrace) {			/* an unsound trace primitive for  */
    fflush(stdout);			/* debugging purposes		   */
    eval(pop());			/*  :: String -> a -> a		   */
    while (whnfHead==nameCons) {
	eval(pop());
	putchar(charOf(whnfHead));
	eval(pop());
    }
    updateRoot(pop());
}

primFun(primMakeMem) {			/* construct member function	   */
    Int  dc = pop();			/* Assume that makeMember redexes  */
    List ds = name(primArg(1)).type;	/* appear only in dictionary blocks*/
    Bool gc = consGC;			/* and need no further evaluation  */
    consGC  = TRUE;
    for (; nonNull(tl(ds)); ds=tl(ds))	/* Also assumes that ds is nonNull */
	toparg(makeDictFor(hd(ds),dc));
    updapRoot(top(),makeDictFor(hd(ds),dc));
    consGC  = gc;
}

primFun(primConCmp) {			/* compare constructors		   */
    eval(primArg(2));			/*  :: a -> a -> Ordering	   */
    if (isName(whnfHead) && isCfun(whnfHead)) {
	Int l = cfunOf(whnfHead);
	eval(primArg(1));
	if (isName(whnfHead) && isCfun(whnfHead)) {
	    Int r = cfunOf(whnfHead);
	    updateRoot(l<r ? nameLT : (l>r ? nameGT : nameEQ));
	    return;
	}
    }
    cantReduce();
}

primFun(primEnRange) {			/* derived range for enum type	   */
    eval(primArg(1));			/* :: (a,a) -> [a]		   */
    updapRoot(ap(nameEnFrTo,primArg(3)),primArg(2));
}

primFun(primEnIndex) {			/* derived index for enum type	   */
    eval(primArg(2));			/*  :: (a,a) -> a -> Int	   */
    eval(primArg(4));			/* evaluate lower bound		   */
    if (isName(whnfHead) && isCfun(whnfHead)) {
	Int l = cfunOf(whnfHead);
	eval(primArg(3));		/* evaluate upper bound		   */
	if (isName(whnfHead) && isCfun(whnfHead)) {
	    Int h = cfunOf(whnfHead);
	    eval(primArg(1));		/* evaluate index		   */
	    if (l<=cfunOf(whnfHead) && cfunOf(whnfHead)<=h) {
		updateRoot(mkInt(cfunOf(whnfHead)-l));
		return;
	    }
	}
    }
    cantReduce();
}

primFun(primEnInRng) {			/* derived inRange for enum type   */
    eval(primArg(2));			/*  :: (a,a) -> a -> Bool	   */
    eval(primArg(4));			/* evaluate lower bound		   */
    if (isName(whnfHead) && isCfun(whnfHead)) {
	Int l = cfunOf(whnfHead);
	eval(primArg(3));		/* evaluate upper bound		   */
	if (isName(whnfHead) && isCfun(whnfHead)) {
	    Int h = cfunOf(whnfHead);
	    eval(primArg(1));		/* evaluate index		   */
	    if (l<=cfunOf(whnfHead) && cfunOf(whnfHead)<=h)
		updateRoot(nameTrue);
	    else
		updateRoot(nameFalse);
	    return;
	}
    }
    cantReduce();
}

primFun(primEnToEn) {			/* derived toEnum for enum type	   */
    Name n;				/* :: a -> Int -> a		   */
    eval(primArg(2));
    n = whnfHead;
    eval(primArg(1));
    if (nonNull(n = cfunByNum(n,whnfInt)))
	updateRoot(n);
    else
	cantReduce();
}

primFun(primEnFrEn) {			/* derived fromEnum for enum type  */
    eval(primArg(1));                  	/* :: a -> Int			   */
    if (isName(whnfHead) && isCfun(whnfHead)) {
	Int i = cfunOf(whnfHead);
	updateRoot(mkInt(i==0 ? 0 : (i-1)));
    }
    else
	cantReduce();
}

primFun(primEnFrom) {			/* derived enumFrom for enum type  */
    eval(primArg(1));			/* :: a -> [a] 			   */
    if (isName(whnfHead) && isCfun(whnfHead)) {
	Name cfs = succCfun(whnfHead);
	push(isNull(cfs) ? nameNil : ap(nameEnFrom,cfs));
	updapRoot(ap(nameCons,whnfHead),top());
    }
    else
	cantReduce();
}

primFun(primEnFrTo) {			/* derived enumFromTo for enum type*/
    eval(primArg(2));			/* :: a -> a -> [a]		   */
    if (isName(whnfHead) && isCfun(whnfHead)) {
	Name l = whnfHead;
	eval(primArg(1));
	if (isName(whnfHead) && isCfun(whnfHead)) {
	    if (cfunOf(l) < cfunOf(whnfHead)) {
		push(ap(nameEnFrTo,succCfun(l)));
		updapRoot(ap(nameCons,l),ap(top(),whnfHead));
	    }
	    else if (l==whnfHead)
		updapRoot(ap(nameCons,l),nameNil);
	    else
		updateRoot(nameNil);
	    return;
	}
    }
    cantReduce();
}

primFun(primEnFrTh) {			/* derived enumFromThen for enum ty*/
   eval(primArg(2));			/* :: a -> a -> [a]		   */
   if (isName(whnfHead) && isCfun(whnfHead)) {
	Name f = whnfHead;
	eval(primArg(1));
	if (isName(whnfHead) && isCfun(whnfHead)) {
	    Name n = nextCfun(f,whnfHead);
	    if (isNull(n)) {
		push(ap(nameCons,whnfHead));
		toparg(nameNil);
	    }
	    else {
		push(ap(nameEnFrTh,whnfHead));
		toparg(n);
	    }
	    updapRoot(ap(nameCons,f),top());
	    return;
	}
    }
    cantReduce();
}

/* --------------------------------------------------------------------------
 * Array primitives:
 * ------------------------------------------------------------------------*/

#if HASKELL_ARRAYS
#include "array.c"
#endif

/* --------------------------------------------------------------------------
 * Integer arithmetic primitives:
 * ------------------------------------------------------------------------*/

primFun(primMinInt) {			/* minimum integer CAF		   */
    push(mkInt((-MAXPOSINT)-1));
}

primFun(primMaxInt) {			/* maximum integer CAF		   */
    push(mkInt(MAXPOSINT));
}

primFun(primPlusInt) {		       /* Integer addition primitive	   */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(mkInt(x+whnfInt));
}

primFun(primMinusInt) { 	       /* Integer subtraction primitive    */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(mkInt(x-whnfInt));
}

primFun(primMulInt) {		       /* Integer multiplication primitive */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(mkInt(x*whnfInt));
}

primFun(primQrmInt) {			/* Integer quotient and remainder  */
    Int x;				/* truncated towards zero	   */
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    if (whnfInt==0)
	cantReduce();
    push(mkInt(x/whnfInt));
    topfun(mkTuple(2));
    updapRoot(top(),mkInt(x%whnfInt));
}

primFun(primQuotInt) {			/* Integer division primitive	   */
    Int x;				/* truncated towards zero	   */
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    if (whnfInt==0)
	cantReduce();
    updateRoot(mkInt(x/whnfInt));
}

primFun(primDivInt) {			/* Integer division primitive	   */
    Int x,r;				/* truncated towards -ve infinity  */
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    if (whnfInt==0)
	cantReduce();
    r = x%whnfInt;
    x = x/whnfInt;
    if ((whnfInt<0 && r>0) || (whnfInt>0 && r<0))
	x--;
    updateRoot(mkInt(x));
}

primFun(primModInt) {		       /* Integer modulo primitive	   */
    Int x,y;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    if (whnfInt==0)
	cantReduce();
    y = x%whnfInt;		       /* "... the modulo having the sign  */
    if ((y<0 && whnfInt>0) ||	       /*	       of the divisor ..." */
	(y>0 && whnfInt<0))	       /* See definition on p.91 of Haskell*/
	updateRoot(mkInt(y+whnfInt));  /* report... (Haskell 1.1?)	   */
    else
	updateRoot(mkInt(y));
}

primFun(primRemInt) {		       /* Integer remainder primitive	   */
    Int x;
    eval(primArg(2));		       /* quot and rem satisfy:		   */
    x = whnfInt;		       /* (x `quot` y)*y + (x `rem` y) == x*/
    eval(primArg(1));		       /* which is exactly the property    */
    if (whnfInt==0)		       /* described in K&R 2:		   */
	cantReduce();		       /*      (a/b)*b + a%b == a	   */
    updateRoot(mkInt(x%whnfInt));
}

primFun(primNegInt) {		       /* Integer negation primitive	   */
    eval(primArg(1));
    updateRoot(mkInt(-whnfInt));
}

primFun(primEvenInt) {		       /* Integer even predicate	   */
    eval(primArg(1));
    updateRoot((whnfInt&1) ? nameFalse : nameTrue);
}

/* --------------------------------------------------------------------------
 * Haskell Integer (bignum) primitives:
 * ------------------------------------------------------------------------*/

#if BIGNUMS
#include "bignums.c"
#endif

/* --------------------------------------------------------------------------
 * Coercion primitives:
 * ------------------------------------------------------------------------*/

primFun(primCharToInt) {	       /* Character to integer primitive   */
    eval(primArg(1));
    updateRoot(mkInt(charOf(whnfHead)));
}

primFun(primIntToChar) {	       /* Integer to character primitive   */
    eval(primArg(1));
    if (whnfInt<0  || whnfInt>MAXCHARVAL)
	cantReduce();
    updateRoot(mkChar(whnfInt));
}

primFun(primIntToFloat) {		/* Integer to Float primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat((Float)(whnfInt)));
}

primFun(primDummyCvt) {			/* dummy (identity) conversion	   */
    updateRoot(primArg(1));
}

/* --------------------------------------------------------------------------
 * Float arithmetic primitives:
 * ------------------------------------------------------------------------*/

primFun(primPlusFloat) {	       /* Float addition primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(mkFloat(x+whnfFloat));
}

primFun(primMinusFloat) { 	       /* Float subtraction primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(mkFloat(x-whnfFloat));
}

primFun(primMulFloat) {		       /* Float multiplication primitive   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(mkFloat(x*whnfFloat));
}

primFun(primDivFloat) {		       /* Float division primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    if (whnfFloat==0)
	cantReduce();
    updateRoot(mkFloat(x/whnfFloat));
}

primFun(primNegFloat) {		       /* Float negation primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(-whnfFloat));
}

#if HAS_FLOATS
primFun(primPiFloat) {			/* Float pi primitive		   */
    push(mkFloat(3.1415926535));
}

primFun(primSinFloat) {			/* Float sin (trig) primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(sin(whnfFloat)));
}

primFun(primCosFloat) {			/* Float cos (trig) primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(cos(whnfFloat)));
}

primFun(primTanFloat) {			/* Float tan (trig) primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(tan(whnfFloat)));
}

primFun(primAsinFloat) {		/* Float arc sin (trig) primitive  */
    eval(primArg(1));
    updateRoot(mkFloat(asin(whnfFloat)));
}

primFun(primAcosFloat) {		/* Float arc cos (trig) primitive  */
    eval(primArg(1));
    updateRoot(mkFloat(acos(whnfFloat)));
}

primFun(primAtanFloat) {		/* Float arc tan (trig) primitive  */
    eval(primArg(1));
    updateRoot(mkFloat(atan(whnfFloat)));
}

primFun(primExpFloat) {			/* Float exponential primitive	   */
    eval(primArg(1));
    updateRoot(mkFloat(exp(whnfFloat)));
}

primFun(primLogFloat) {			/* Float logarithm primitive	   */
    eval(primArg(1));
    if (whnfFloat<=0)
	cantReduce();
    updateRoot(mkFloat(log(whnfFloat)));
}

primFun(primSqrtFloat) {		/* Float square root primitive	   */
    eval(primArg(1));
    if (whnfFloat<0)
	cantReduce();
    updateRoot(mkFloat(sqrt(whnfFloat)));
}

primFun(primFloatToInt) {		/* Adhoc Float --> Int conversion  */
    eval(primArg(1));
    updateRoot(mkInt((Int)(whnfFloat)));
}

primFun(primFloatRadix) {		/* Float radix primitive	   */
#if BIGNUMS				/*  :: a -> Integer		   */
    updateRoot(bigInt(FLT_RADIX));	/* from K&R2, I hope it's portable */
#else
    updateRoot(mkInt(FLT_RADIX));
#endif
}

primFun(primFloatDigits) {		/* Float sig. digits primitive	   */
    updateRoot(mkInt(FLT_MANT_DIG));	/*  :: a -> Int			   */
}					/* again, courtesy K&R2		   */

primFun(primFloatRange) {		/* Float exponent range primitive  */
    push(mkInt(FLT_MIN_EXP));
    updapRoot(ap(mkTuple(2),top()),mkInt(FLT_MAX_EXP));
}

primFun(primFloatDecode) {		/* Float decode primitive	   */
    double f;				/*  :: Float -> (Integer,Int)	   */
    Int    n;				/* another gruesome hack	   */
    eval(primArg(1));
    f  = frexp((double)(whnfFloat),&n);	/* 0.5   <= f < 1		   */
    f  = ldexp(f,FLT_MANT_DIG);		/* 2^m-1 <= f < 2^m, m=FLT_MANT_DIG*/
    n -= FLT_MANT_DIG;
#if BIGNUMS
    push(bigDouble(f));
#else
    push(mkInt((Int)f));
#endif
    updapRoot(ap(mkTuple(2),top()),mkInt(n));
}

primFun(primFloatEncode) {		/* Float encode primitive	   */
    Int    n;				/*  :: Integer -> Int -> a	   */
    double f;				/* Ugly hack, don't use Hugs for   */
    eval(primArg(1));			/* numerical work		   */
    n = whnfInt;
    eval(primArg(2));			/* get integer			   */
#if DJGPP2
    _fpreset();				/* Get round a possible DJGPP bug? */
#endif
#if BIGNUMS
    f = floatOf(bigToFloat(whnfHead));	/* and turn it into a float	   */
#else
    f = (double)(whnfInt);
#endif
    updateRoot(mkFloat(ldexp(f,n)));
}
#endif

/* --------------------------------------------------------------------------
 * Comparison primitives:
 * ------------------------------------------------------------------------*/

primFun(primEqInt) {		       /* Integer equality primitive	   */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(x==whnfInt ? nameTrue : nameFalse);
}

primFun(primCmpInt) {		       /* Integer compare primitive	   */
    Int x;
    eval(primArg(2));
    x = whnfInt;
    eval(primArg(1));
    updateRoot(x<whnfInt ? nameLT : (x>whnfInt ? nameGT : nameEQ));
}

primFun(primEqChar) {		       /* Character equality primitive	   */
    Cell x;
    eval(primArg(2));
    x = whnfHead;
    eval(primArg(1));
    updateRoot(x==whnfHead ? nameTrue : nameFalse);
}

primFun(primCmpChar) {		       /* Character compare primitive	   */
    Cell x;
    eval(primArg(2));
    x = whnfHead;
    eval(primArg(1));
    updateRoot(x<whnfHead ? nameLT : (x>whnfHead ? nameGT : nameEQ));
}

primFun(primEqFloat) {		       /* Float equality primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(x==whnfFloat ? nameTrue : nameFalse);
}

primFun(primCmpFloat) {		       /* Float compare primitive	   */
    Float x;
    eval(primArg(2));
    x = whnfFloat;
    eval(primArg(1));
    updateRoot(x<whnfFloat ? nameLT : (x>whnfFloat ? nameGT : nameEQ));
}

/* --------------------------------------------------------------------------
 * Print primitives:
 * ------------------------------------------------------------------------*/

#include "printer.c"

/* --------------------------------------------------------------------------
 * Evaluate name, obtaining a C string from a Hugs string:
 * ------------------------------------------------------------------------*/

static String local evalName(es)	/* evaluate es :: [Char] and save  */
Cell es; {				/* in char array... return ptr to  */
    static char buffer[FILENAME_MAX+1];	/* string or 0, if error occurs	   */
    Int         pos    = 0;
    StackPtr    saveSp = sp;

    eval(es);
    while (whnfHead==nameCons && pos<FILENAME_MAX) {
	eval(pop());
	buffer[pos++] = charOf(whnfHead);
	eval(pop());
    }
    if (whnfHead==nameNil) {
	buffer[pos] = '\0';
	return buffer;
    }
    sp = saveSp;			/* stack pointer must be the same  */
    return 0;				/* as it was on entry		   */
}

/* --------------------------------------------------------------------------
 * Top-level printing mechanism:
 * ------------------------------------------------------------------------*/

Void outputString(fp)			/* Evaluate string on top of stack */
FILE *fp; {				/* and print it on fp		   */
    StackPtr origSp = sp;
    for (;;) {
	Cell temp = evalWithNoError(pop());
	if (nonNull(temp)) {
	    sp    = origSp;
	    top() = printBadRedex((top()=temp),nameNil);
	}
	else if (whnfHead==nameCons) {
	    if (nonNull(temp=evalWithNoError(pop()))) {
		sp        = origSp;
		onto(temp);
		pushed(1) = printBadRedex(pushed(0),pushed(1));
		drop();
	    }
	    else {
		putc(charOf(whnfHead),fp);
		fflush(fp);
	    }
	}
	else
	    return;
    }
}

/* --------------------------------------------------------------------------
 * IO monad implementation
 * ------------------------------------------------------------------------*/

#if IO_MONAD
#include "iomonad.c"
#endif

/* --------------------------------------------------------------------------
 * Mutable variables,  based on `Lazy State Threads' by Launchbury and
 * Peyton Jones, PLDI 94.
 *
 * type ST s a = State s -> (a, State s)
 * ------------------------------------------------------------------------*/

#if LAZY_ST
primFun(primSTRun) {			/* ST monad encapsulate		   */
    updapRoot(nameFst,			/*  :: all s.(ST s a) -> a	   */
	      ap(primArg(1),nameUnit));
}

primFun(primSTReturn) {			/* ST monad return		   */
    updapRoot(mkTuple(2),primArg(1));	/* return    :: a -> ST s a	   */
}					/* return a   = \s -> (a,s)	   */

primFun(primSTBind) {			/* ST monad bind		   */
    Cell r = ap(primArg(3),primArg(1));	/* :: ST s a ->			   */
    push(r);				/*     (a -> ST s b) ->		   */
    topfun(nameFst);			/*	ST s b			   */
    updapRoot(ap(primArg(2),top()),	/* lazy version of bind on ST	   */
	      ap(nameSnd,r));
}

primFun(primSTInter) {			/* ST monad interleave		   */
    push(ap(primArg(2),primArg(1)));	/*  :: ST s a ->		   */
    topfun(nameFst);			/*      ST s a			   */
    updapRoot(ap(mkTuple(2),top()),primArg(1));
}

primFun(primSTNew) {			/* ST monad variable allocator	   */
    eval(primArg(1));			/*  :: a ->			   */
    push(ap(MUTVAR,primArg(2)));	/*	ST s (MutVar s a)	   */
    updapRoot(ap(mkTuple(2),top()),primArg(1));
}

primFun(primSTAssign) {			/* ST monad assignment		   */
    eval(primArg(1));			/*  :: MutVar s a -> a -> ST s ()  */
    eval(primArg(3));
    if (!isPair(whnfHead) || fst(whnfHead)!=MUTVAR)
	internal("type error in assign");
    snd(whnfHead) = primArg(2);		/* Arrgh! impurity! :-)		   */
    updapRoot(ap(mkTuple(2),nameUnit),primArg(1));
}

primFun(primSTDeref) {			/* ST monad dereference		   */
    eval(primArg(1));			/*  :: MutVar s a -> ST s a	   */
    eval(primArg(2));
    if (!isPair(whnfHead) || fst(whnfHead)!=MUTVAR)
	internal("type error in deref");
    updapRoot(ap(mkTuple(2),snd(whnfHead)),primArg(1));
}

primFun(primSTMutVarEq) {		/* ST monad variable equality	   */
    Cell x;				/*  :: MutVar s a -> 		   */
    eval(primArg(2));   		/*	MutVar s a -> Bool	   */
    x = whnfHead;
    eval(primArg(1));
    updateRoot(x==whnfHead ? nameTrue : nameFalse);
}

/* See also: implementation of ST primitives for mutable arrays in array.c */
#endif

/* --------------------------------------------------------------------------
 * Build array of character conses:
 * ------------------------------------------------------------------------*/

static Cell consCharArray[NUM_CHARS];

Cell consChar(c)			/* return application (:) c	   */
Char c; {
    if (c<0)
	c += NUM_CHARS;
    return consCharArray[c];
}

/* --------------------------------------------------------------------------
 * Built-in control:
 * ------------------------------------------------------------------------*/

Void builtIn(what)
Int what; {
    Int i;

    switch (what) {
	case RESET   : if (writingFile) {
			   fclose(writingFile);
			   writingFile = 0;
		       }
		       out = NIL;
		       bn  = NIL;
		       break;

	case MARK    : for (i=0; i<NUM_CHARS; ++i)
			   mark(consCharArray[i]);
		       mark(out);
		       mark(bn);
		       break;

	case INSTALL : for (i=0; i<NUM_CHARS; ++i)
			   consCharArray[i] = ap(nameCons,mkChar(i));
		       out = NIL;
		       bn  = NIL;

#define pFun(n,s,t)    addPrim(0,n=newName(findText(s)),t,NIL)
		       pFun(nameFatbar,	   "_FATBAR",  "fatbar");
		       pFun(nameFail,	   "_FAIL",    "fail");
		       pFun(nameIf,	   "_IF",      "if");
		       pFun(nameSel,	   "_SEL",     "sel");

		       pFun(nameIStrict,   "_strict",  "strict");
		       pFun(nameISeq,      "_seq",     "seq");

		       pFun(namePrint,	   "_print",   "print");
		       pFun(nameNPrint,	   "_nprint",  "nprint");
		       pFun(nameLPrint,	   "_lprint",  "lprint");
		       pFun(nameNLPrint,   "_nlprint", "nlprint");
		       pFun(nameSPrint,	   "_sprint",  "sprint");
		       pFun(nameNSPrint,   "_nsprint", "nsprint");

		       pFun(nameConCmp,	   "_concmp",  "conCmp");
		       pFun(nameEnRange,   "_range",   "enRange");
		       pFun(nameEnIndex,   "_index",   "enIndex");
		       pFun(nameEnInRng,   "_inRange", "enInRng");
		       pFun(nameEnToEn,	   "_ToEnum",  "enToEn");
		       pFun(nameEnFrEn,    "_FrEnum",  "enFrEn");
		       pFun(nameEnFrom,    "_From",    "enFrom");
		       pFun(nameEnFrTo,	   "_FromTo",  "enFrTo");
		       pFun(nameEnFrTh,	   "_FromThen","enFrTh");

		       pFun(nameUndefMem,  "_undefined_member", "undefMem");
		       pFun(nameMakeMem,   "_makeMember",    "makeMem");
		       pFun(nameBlackHole, "_Gc Black Hole", "gcBhole");
#if    HASKELL_ARRAYS
		       pFun(nameEltUndef,  "_undefined_array_element",
							"eltUndef");
		       pFun(nameOutBounds, "_out_of_bounds","outBounds");
#endif
#if    IO_MONAD
		       pFun(nameUncaught,  "_uncaught",	"uncaughtIO");
		       pFun(nameDone,	   "_done",	"doneIO");
		       pFun(namePass,	   "_pass",	"passIO");
		       pFun(nameHreader,   "_hreader", "hreader");
#endif
#if    LAZY_ST
		       pFun(nameSTRun,	   "runST",	"STRun");
#endif
#undef pFun
#define predef(nm,str) nm=newName(findText(str)); name(nm).defn=PREDEFINED
		       predef(nameNegate,	"negate");
		       predef(nameFlip,		"flip");
		       predef(nameFrom,		"enumFrom");
		       predef(nameFromThen,	"enumFromThen");
		       predef(nameFromTo,	"enumFromTo");
		       predef(nameFromThenTo,	"enumFromThenTo");
		       predef(nameAnd,		"&&");
		       predef(nameOr,		"||");
		       predef(nameFst,		"fst");
		       predef(nameSnd,		"snd");
		       predef(nameId,		"id");
		       predef(nameOtherwise,	"otherwise");
		       predef(nameError,	"error");
		       predef(nameUndefined,	"undefined");
		       predef(nameComp,		".");
		       predef(nameApp,		"++");
		       predef(nameShowField,	"showField");
		       predef(nameShowParen,	"showParen");
		       predef(nameRangeSize,	"rangeSize");
		       predef(nameCompAux,	"primCompAux");
		       predef(namePmInt,	"primPmInt");
		       predef(namePmInteger,	"primPmInteger");
		       predef(namePmFlt,	"primPmFlt");
#if NPLUSK
		       predef(namePmNpk,	"primPmNpk");
		       predef(namePmSub,	"primPmSub");
#endif
#undef  predef
		       break;
    }
}

/*-------------------------------------------------------------------------*/
