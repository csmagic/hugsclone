/* --------------------------------------------------------------------------
 * connect.h:	Copyright (c) Mark P Jones 1991-1996.   All rights reserved.
 *		See NOTICE for details and conditions of use etc...
 *              Hugs version 1.3, August 1996
 *
 * Connections between components of the Hugs system
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Standard data:
 * ------------------------------------------------------------------------*/

extern Name  nameFalse,	  nameTrue;	/* primitive constructor functions */
extern Name  nameNil,	  nameCons;
extern Name  nameJust,	  nameNothing;
extern Name  nameUnit;
extern Name  nameLT,      nameEQ;
extern Name  nameGT;
extern Name  nameFst,	  nameSnd;	/* standard combinators		   */
extern Name  nameId,	  nameOtherwise;
extern Name  nameNegate,  nameFlip;	/* primitives reqd for parsing	   */
extern Name  nameFrom,    nameFromThen;
extern Name  nameFromTo,  nameFromThenTo;
extern Name  nameFatbar,  nameFail;	/* primitives reqd for translation */
extern Name  nameIf,	  nameSel;
extern Name  nameCompAux;
extern Name  namePmInt,	  namePmFlt;	/* primitives for pattern matching */
extern Name  namePmInteger;
#if NPLUSK
extern Name  namePmNpk,	  namePmSub;	/* primitives for (n+k) patterns   */
#endif
extern Name  nameUndefMem;	 	/* undefined member primitive	   */
extern Name  nameMakeMem;		/* makeMember primitive		   */
extern Name  nameError;			/* For runtime error messages	   */
extern Name  nameUndefined;		/* A generic undefined value	   */
extern Name  nameBlackHole;		/* for GC-detected black hole	   */
extern Name  nameAnd,	  nameOr;	/* for optimisation of && and ||   */
extern Name  nameFromInt, nameFromDouble;/*coercion of numerics		   */
extern Name  nameFromInteger;
extern Name  nameEq,	  nameCompare;	/* names used for deriving	   */
extern Name  nameMinBnd,  nameMaxBnd;
extern Name  nameIndex,	  nameInRange;
extern Name  nameRange;
extern Name  nameLe,	  nameShowsPrec;
extern Name  nameMult,	  namePlus;
extern Name  nameConCmp,  nameEnRange;
extern Name  nameEnIndex, nameEnInRng;
extern Name  nameEnToEn,  nameEnFrEn;
extern Name  nameEnFrom,  nameEnFrTh;
extern Name  nameEnFrTo;
extern Name  nameComp,	  nameApp;	/* composition and append	   */
extern Name  nameShowField;		/* display single field		   */
extern Name  nameShowParen;		/* wrap with parens		   */
extern Name  nameRangeSize;		/* calculate size of index range   */
extern Class classMonad,  classMonad0;	/* Monads and monads with a zero   */
extern Name  nameReturn,  nameBind;	/* for translating monad comps	   */
extern Name  nameZero;			/* for monads with a zero	   */

extern Name  nameStrict,  nameSeq;	/* Members of class Eval	   */
extern Name  nameIStrict, nameISeq;	/* ... and their implementations   */

extern Name  namePrint;			/* printing primitive		   */

#if    IO_MONAD
extern Type   typeProgIO;		/* For the IO monad, IO ()	   */
extern Void   ioExecute Args((Void));	/* IO monad executor		   */
extern Name   nameUserErr,  nameIllegal;/* primitives required for IOError */
extern Name   nameNameErr,  nameSearchErr;
extern Name   nameWriteErr, nameEvalErr;
#endif
#if    LAZY_ST
extern Type   typeST;			/* Lazy state threads		   */
extern Name   nameSTRun;		/* Encapsulator			   */
#endif
#if    NPLUSK
extern Text  textPlus;			/* Used to recognise n+k patterns  */
#endif

extern String repeatStr;		/* Repeat last command string	   */
extern String hugsEdit;			/* String for editor command	   */
extern String hugsPath;			/* String for file search path	   */

extern Type  typeArrow;			/* Builtin type constructors	   */
extern Type  typeList;
extern Type  typeUnit;

extern List  stdDefaults;		/* List of standard default types  */

extern Class classEq;			/* `standard' classes		   */
extern Class classOrd;
extern Class classShow;
extern Class classRead;
extern Class classIx;
extern Class classEnum;
extern Class classEval;
extern Class classBounded;

extern Class classReal;			/* `numeric' classes		   */
extern Class classIntegral;
extern Class classRealFrac;
extern Class classRealFloat;
extern Class classFractional;
extern Class classFloating;
extern Class classNum;

extern Cell  *CStackBase;		/* pointer to base of C stack	   */

extern List  tyconDefns;		/* list of type constructor defns  */
extern List  typeInDefns;		/* list of synonym restrictions	   */
extern List  valDefns;			/* list of value definitions       */
extern List  opDefns;			/* list of operator definitions    */
extern List  classDefns;		/* list of class definitions       */
extern List  instDefns;			/* list of instance definitions    */
extern List  selDefns;			/* list of selector lists	   */
extern List  overDefns;			/* list of overloaded member defns */
extern List  primDefns;			/* list of primitive definitions   */
extern List  defaultDefns;		/* default definitions (if any)	   */
extern Int   defaultLine;		/* line in which default defs occur*/
extern List  evalDefaults;		/* defaults for evaluator	   */
extern Cell  inputExpr;			/* evaluator input expression      */
extern Addr  inputCode;			/* Code for compiled input expr    */

extern Int   whnfArgs;			/* number of args of term in whnf  */
extern Cell  whnfHead;			/* head of term in whnf            */
extern Int   whnfInt;			/* integer value of term in whnf   */
extern Float whnfFloat;			/* float value of term in whnf	   */
extern Long  numReductions;		/* number of reductions used       */
extern Long  numCells;			/* number of cells allocated       */
extern Int   numberGcs;			/* number of garbage collections   */
extern Int   cellsRecovered;		/* cells recovered by last gc	   */
extern Bool  broken;			/* indicates interrupt received    */

extern Bool  gcMessages;		/* TRUE => print GC messages	   */
extern Bool  literateScripts;		/* TRUE => default lit scripts     */
extern Bool  literateErrors;		/* TRUE => report errs in lit scrs */
extern Bool  failOnError;		/* TRUE => error produces immediate*/
					/*	   termination		   */
extern Bool  kindExpert;		/* TRUE => display kind errors in  */
					/* 	   full detail		   */

/* --------------------------------------------------------------------------
 * Function prototypes etc...
 * ------------------------------------------------------------------------*/

extern Void   everybody        Args((Int));

#define RESET   1		/* reset subsystem                         */
#define MARK    2		/* mark parts of graph in use by subsystem */
#define INSTALL 3		/* install subsystem (executed once only)  */
#define EXIT	4		/* Take action immediately before exit()   */
#define BREAK   5		/* Take action after program break	   */

typedef long   Target;
extern  Void   setGoal          Args((String, Target));
extern  Void   soFar            Args((Target));
extern  Void   done             Args((Void));
extern  String fromEnv		Args((String,String));
extern  Bool   chase		Args((List));

extern  Void   storage          Args((Int));
extern  Void   setLastExpr	Args((Cell));
extern  Cell   getLastExpr	Args((Void));
extern  List   addTyconsMatching Args((String,List));
extern	List   addNamesMatching Args((String,List));

extern  Void   input            Args((Int));
extern  Void   consoleInput     Args((String));
extern  Void   projInput	Args((String));
extern  Void   parseScript      Args((String,Long));
extern  Void   parseExp         Args((Void));
extern  String readFilename     Args((Void));
extern  String readLine		Args((Void));
extern  Syntax defaultSyntax    Args((Text));
extern  String unlexChar        Args((Char,Char));
extern  Void   printString	Args((String));

extern  Void   staticAnalysis	Args((Int));
extern  Void   tyconDefn	Args((Int,Cell,Cell,Cell));
extern  Void   setTypeIns	Args((List));
extern  Void   clearTypeIns	Args((Void));
extern  Type   fullExpand	Args((Type));
extern  Bool   isAmbiguous	Args((Type));
extern  Void   ambigError	Args((Int,String,Cell,Type));
extern  Void   classDefn	Args((Int,Cell,Cell));
extern  Void   instDefn		Args((Int,Cell,Cell));
extern  Void   addTupInst	Args((Class,Int));
extern  Void   addEvalInst	Args((Int,Cell,Int,List));
extern  Void   primDefn		Args((Cell,List,Cell));
extern  Void   defaultDefn	Args((Int,List));
extern  Void   checkExp		Args((Void));
extern  Void   checkDefns	Args((Void));

extern  Void   typeChecker	Args((Int));
extern  Type   typeCheckExp	Args((Bool));
extern  Cell   getDictFor	Args((Class,Type));
extern  Void   typeCheckDefns	Args((Void));
extern  Cell   rhsExpr		Args((Cell));
extern  Int    rhsLine		Args((Cell));
extern  Bool   typeMatches	Args((Type,Type));
extern  Cell   superEvid	Args((Cell,Class,Class));
extern  Bool   mtInst		Args((Class,Type));
extern  Cell   makeDictFor      Args((Cell,Cell));
extern  Void   linkPreludeTC    Args((Void));
extern  Void   linkPreludeCM    Args((Void));
extern  Void   kindTCGroup	Args((List));
extern  Void   kindSigType	Args((Int,Type));
extern  Void   kindInst		Args((Inst,Cell));
extern  Void   kindDefaults	Args((Int,List));

extern  Void   compiler         Args((Cell));
extern  Void   compileDefns     Args((Void));
extern  Void   compileExp       Args((Void));
extern  Bool   failFree		Args((Cell));
extern  Int    discrArity       Args((Cell));

extern  Void   machine          Args((Int));
extern  Addr   codeGen          Args((Name,Int,Cell));
extern  Void   implementCfun	Args((Name,List));;
extern  Void   implementSfun	Args((Name));
extern  Void   externalPrim	Args((Name,String));
extern  Void   addCfunTable	Args((Tycon));
extern  Name   succCfun		Args((Name));
extern  Name   nextCfun		Args((Name,Name));
extern  Name   cfunByNum	Args((Name,Int));
extern  Void   unwind           Args((Cell));
extern  Void   eval             Args((Cell));
extern  Cell   evalWithNoError  Args((Cell));
extern  Void   evalFails        Args((StackPtr));
extern  Void   graphForExp	Args((Void));

extern  Void   builtIn          Args((Int));
extern  Void   abandon		Args((String,Cell));
extern  Void   outputString	Args((FILE *));
extern  Void   dialogue		Args((Cell));
extern  Cell   consChar		Args((Char));
#if BIGNUMS
extern  Bignum bigInt		Args((Int));
extern  Bignum bigDouble	Args((double));
extern  Cell   bigToInt		Args((Bignum));
extern  Cell   bigToFloat	Args((Bignum));
extern  Bignum bigStr		Args((String));
extern  Cell   bigOut		Args((Bignum,Cell,Bool));
extern  Bignum bigShift		Args((Bignum,Int,Int));
extern  Int    bigCmp		Args((Bignum,Bignum));
#endif

extern  Void   machdep          Args((Int));
extern  String findPathname	Args((String,String));
#if PROFILING
extern  String timeString	Args((Void));
#endif

extern  Bool   startEdit	Args((Int,String));
extern  Int    shellEsc		Args((String));
extern  Int    getTerminalWidth Args((Void));
extern  Void   normalTerminal	Args((Void));
extern  Void   noechoTerminal	Args((Void));
extern  Int    readTerminalChar Args((Void));
extern  Void   gcStarted	Args((Void));
extern  Void   gcScanning	Args((Void));
extern  Void   gcRecovered	Args((Int,Int));
extern  Void   gcCStack		Args((Void));

/*-------------------------------------------------------------------------*/
