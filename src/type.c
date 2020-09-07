/* --------------------------------------------------------------------------
 * type.c:      Copyright (c) Mark P Jones 1991-1996.   All rights reserved.
 *              See NOTICE for details and conditions of use etc...
 *              Hugs version 1.3, August 1996.
 *
 * This is the Hugs type checker
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"

/*#define DEBUG_TYPES*/
/*#define DEBUG_KINDS*/
/*#define DEBUG_DICTS*/
/*#define DEBUG_DEFAULTS*/
/*#define DEBUG_SELS*/

Bool catchAmbigs       = FALSE;		/* TRUE => functions with ambig.   */
					/* 	   types produce error	   */

Type typeArrow,   typeList;		/* Important primitive types	   */
Type typeUnit;

static Type typeInt,     typeDouble;
static Type typeString,  typeChar;
static Type typeBool,    typeMaybe;
static Type typeOrdering;

Class classEq,    classOrd;		/* `standard' classes		   */
Class classIx,    classEnum;
Class classShow,  classRead;
Class classEval,  classBounded;

Class classReal,       classIntegral;	/* `numeric' classes		   */
Class classRealFrac,   classRealFloat;
Class classFractional, classFloating;
Class classNum;

List stdDefaults;			/* standard default values	   */

Name nameFromInt, nameFromDouble;	/* coercion of numerics		   */
Name nameFromInteger;
Name nameEq,      nameCompare;		/* derivable names		   */
Name nameLe,	  nameShowsPrec;
Name nameMinBnd,  nameMaxBnd;
Name nameIndex,	  nameInRange;
Name nameRange;
Name nameMult,	  namePlus;
Name nameTrue,	  nameFalse;		/* primitive boolean constructors  */
Name nameNil,     nameCons;		/* primitive list constructors	   */
Name nameJust,	  nameNothing;		/* primitive Maybe constructors	   */
Name nameUnit;				/* primitive Unit type constructor */
Name nameLT,	  nameEQ;		/* Ordering constructors	   */
Name nameGT;
Class classMonad, classMonad0;		/* Monads and monads with a zero   */
Name nameReturn,  nameBind;		/* for translating monad comps	   */
Name nameZero;				/* for monads with a zero	   */
Name nameStrict,  nameSeq;		/* Members of class Eval	   */

#if    IO_MONAD
Type   typeProgIO;			/* For the IO monad, IO ()	   */
Name   nameUserErr,  nameIllegal;	/* loosely coupled IOError cfuns   */
Name   nameNameErr,  nameSearchErr;
Name   nameWriteErr, nameEvalErr;
#endif

#if    LAZY_ST
Type   typeST;				/* Lazy state threads		   */
#endif

/* --------------------------------------------------------------------------
 * Data structures for storing a substitution:
 *
 * For various reasons, this implementation uses structure sharing, instead of
 * a copying approach.	In principal, this is fast and avoids the need to
 * build new type expressions.	Unfortunately, this implementation will not
 * be able to handle *very* large expressions.
 *
 * The substitution is represented by an array of type variables each of
 * which is a triple:
 *	bound	a (skeletal) type expression, or NIL if the variable
 *		is not bound.
 *	offs	offset of skeleton in bound.  If isNull(bound), then offs is
 *		used to indicate whether that variable is generic (i.e. free
 *		in the current assumption set) or fixed (i.e. bound in the
 *		current assumption set).  Generic variables are assigned
 *		offset numbers whilst copying type expressions (t,o) to
 *		obtain their most general form.
 *	kind	kind of value bound to type variable (`type variable' is
 *		rather inaccurate -- `constructor variable' would be better).
 * ------------------------------------------------------------------------*/

typedef struct {			/* Each type variable contains:	   */
    Type bound;				/* A type skeleton (unbound==NIL)  */
    Int  offs;				/* Offset for skeleton		   */
    Kind kind;				/* kind annotation		   */
} Tyvar;

static	Int	   numTyvars;		/* no. type vars currently in use  */
#if     FIXED_SUBST
static	Tyvar	   tyvars[NUM_TYVARS];	/* storage for type variables	   */
#else
static  Tyvar     *tyvars = 0;		/* storage for type variables	   */
static  Int        maxTyvars = 0;
#endif
static	Int	   typeOff;		/* offset of result type 	   */
static	Type	   typeIs;		/* skeleton of result type	   */
static	List	   predsAre;		/* list of predicates in type	   */
#define tyvar(n)   (tyvars+(n))		/* nth type variable		   */
#define tyvNum(t)  ((t)-tyvars)		/* and the corresp. inverse funct. */
#define isBound(t) ((t)->bound)
static	Int	   nextGeneric;	        /* number of generics found so far */
static  List	   genericVars;		/* list of generic vars		   */

					/* offs values when isNull(bound): */
#define FIXED_TYVAR    0	        /* fixed in current assumption	   */
#define UNUSED_GENERIC 1	        /* not fixed, not yet encountered  */
#define GENERIC        2	        /* GENERIC+n==nth generic var found*/

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void   local emptySubstitution Args((Void));
static Void   local expandSubst       Args((Int));
static Int    local newTyvars         Args((Int));
static Int    local newKindedVars     Args((Kind,Bool));
static Tyvar *local getTypeVar        Args((Type,Int));
static Void   local tyvarType         Args((Int));
static Void   local bindTv            Args((Int,Type,Int));
static Void   local expandSyn	      Args((Tycon, Int, Type *, Int *));
static Void   local expandSyn1	      Args((Tycon, Type *, Int *));
static Cell   local getDerefHead      Args((Type,Int));

static Void   local clearMarks        Args((Void));
static Void   local resetGenericsFrom Args((Int));
static Void   local markTyvar         Args((Int));
static Void   local markType          Args((Type,Int));

static Type   local copyTyvar         Args((Int));
static Type   local copyType          Args((Type,Int));
static List   local genvarTyvar	      Args((Int,List));
static List   local genvarType	      Args((Type,Int,List));
#ifdef DEBUG_TYPES
static Type   local debugTyvar	      Args((Int));
static Type   local debugType	      Args((Type,Int));
#endif

static Bool   local doesntOccurIn     Args((Type,Int));

static Bool   local varToVarBind      Args((Tyvar *,Tyvar *));
static Bool   local varToTypeBind     Args((Tyvar *,Type,Int));
static Bool   local kvarToVarBind     Args((Tyvar *,Tyvar *));
static Bool   local kvarToTypeBind    Args((Tyvar *,Type,Int));
static Bool   local unify             Args((Type,Int,Type,Int));
static Bool   local sameType          Args((Type,Int,Type,Int));
static Bool   local kunify	      Args((Kind,Int,Kind,Int));

static Void   local kindError	      Args((Int,Constr,Constr,String,Kind,Int));
static Void   local kindConstr	      Args((Int,Constr));
static Kind   local kindAtom	      Args((Constr));
static Void   local kindPred	      Args((Int,Cell));
static Void   local kindType	      Args((Int,String,Type));
static Void   local fixKinds	      Args((Void));

static Void   local initTCKind	      Args((Cell));
static Void   local kindTC	      Args((Cell));
static Void   local genTC	      Args((Cell));
static Kind   local copyKindvar	      Args((Int));
static Kind   local copyKind	      Args((Kind,Int));

static Bool   local eqKind	      Args((Kind,Kind));
static Kind   local getKind	      Args((Cell,Int));

static Kind   local makeSimpleKind    Args((Int));
static Kind   local simpleKind	      Args((Int));
static Kind   local makeVarKind	      Args((Int));
static Void   local varKind	      Args((Int));

static Void   local emptyAssumption   Args((Void));
static Void   local enterBindings     Args((Void));
static Void   local leaveBindings     Args((Void));
static Int    local defType	      Args((Cell));
static Type   local useType	      Args((Cell));
static Void   local markAssumList     Args((List));
static Cell   local findAssum         Args((Text));
static Pair   local findInAssumList   Args((Text,List));
static List   local intsIntersect     Args((List,List));
static List   local genvarAllAss      Args((List));
static List   local genvarAnyAss      Args((List));
static Int    local newVarsBind       Args((Cell));
static Void   local newDefnBind       Args((Cell,Type));
static Void   local instantiate       Args((Type));

static Void   local typeError         Args((Int,Cell,Cell,String,Type,Int));
static Void   local reportTypeError   Args((Int,Cell,Cell,String,Type,Type));
static Cell   local typeExpr          Args((Int,Cell));
static Cell   local varIntro          Args((Cell,Type));
#if LAZY_ST
static Void   local typeRunST	      Args((Int,Cell));
#endif
static Void   local typeEsign         Args((Int,Cell));
static Void   local typeCase          Args((Int,Int,Cell));
static Void   local typeComp	      Args((Int,Type,Cell,List));
static Void   local typeDo	      Args((Int,Cell));
static Cell   local compZero	      Args((List,Int));
static Void   local typeConFlds	      Args((Int,Cell));
static Void   local typeUpdFlds	      Args((Int,Cell));
static Cell   local typeFreshPat      Args((Int,Cell));

static Cell   local typeAp            Args((Int,Cell));
static Void   local typeAlt           Args((Cell));
static Int    local funcType          Args((Int));

static Void   local typeTuple         Args((Cell));
static Type   local makeTupleType     Args((Int));

static Void   local typeBindings      Args((List));
static Void   local removeTypeSigs    Args((Cell));

static Void   local monorestrict      Args((List));
static Void   local restrictedBindAss Args((Cell));
static Void   local restrictedAss     Args((Int,Cell,Type));

static Void   local unrestricted      Args((List));
static Void   local addEvidParams     Args((List,Cell));

static Void   local typeInstDefn      Args((Inst));
static Void   local typeClassDefn     Args((Class));
static Void   local typeMembers	      Args((String,List,List,List,Int,List,Type));
static Void   local typeMember	      Args((String,Name,Name,List,Int,List,Type));

static Void   local typeBind          Args((Cell));
static Void   local typeDefAlt        Args((Int,Cell,Pair));
static Cell   local typeRhs           Args((Cell));
static Void   local guardedType       Args((Int,Cell));

static Void   local genBind	      Args((List,List,Cell));
static Void   local genAss	      Args((Int,List,List,Cell,Type));
static Type   local generalize	      Args((List,Type));
static Void   local tooGeneral        Args((Int,Cell,Type,Type));

static Bool   local checkSchemes      Args((Type,Type));
static Bool   local checkQuals        Args((List,List));
static Bool   local equalTypes        Args((Type,Type));

static Void   local typeDefnGroup     Args((List));
static Void   local typeSel	      Args((Name));

/* --------------------------------------------------------------------------
 * Frequently used type skeletons:
 * ------------------------------------------------------------------------*/

static Type  var;			/* mkOffset(0)		   	   */
static Type  arrow;			/* mkOffset(0) -> mkOffset(1)      */
static Type  boundPair;			/* (mkOffset(0),mkOffset(0))	   */
static Type  listof;			/* [ mkOffset(0) ] 	    	   */
static Type  typeVarToVar;		/* mkOffset(0) -> mkOffset(0)  	   */
#if    LAZY_ST
static Type  typeSTab;			/* ST a b			   */
#endif

static Cell  predNum;			/* Num (mkOffset(0))		   */
static Cell  predFractional;		/* Fractional (mkOffset(0))	   */
static Cell  predIntegral;		/* Integral (mkOffset(0))	   */
static Kind  starToStar;		/* Type -> Type			   */
static Cell  predMonad;			/* Monad (mkOffset(0))		   */
static Cell  predMonad0;		/* Monad0 (mkOffset(0))		   */
static Cell  evalDict;			/* Dictionary for eval instances   */

#define fn(from,to)  ap(ap(typeArrow,from),to)	/* make type:  from -> to  */

/* --------------------------------------------------------------------------
 * Basic operations on current substitution:
 * ------------------------------------------------------------------------*/

#include "subst.c"

/* --------------------------------------------------------------------------
 * Kind expressions:
 *
 * In the same way that values have types, type constructors (and more
 * generally, expressions built from such constructors) have kinds.
 * The syntax of kinds in the current implementation is very simple:
 *
 *	  kind ::= STAR		-- the kind of types
 *		|  kind => kind -- constructors
 *		|  variables	-- either INTCELL or OFFSET
 *
 * ------------------------------------------------------------------------*/

#include "kind.c"

/* --------------------------------------------------------------------------
 * Assumptions:
 *
 * A basic typing statement is a pair (Var,Type) and an assumption contains
 * an ordered list of basic typing statements in which the type for a given
 * variable is given by the most recently added assumption about that var.
 *
 * In practice, the assumption set is split between a pair of lists, one
 * holding assumptions for vars defined in bindings, the other for vars
 * defined in patterns/binding parameters etc.	The reason for this
 * separation is that vars defined in bindings may be overloaded (with the
 * overloading being unknown until the whole binding is typed), whereas the
 * vars defined in patterns have no overloading.  A form of dependency
 * analysis (at least as far as calculating dependents within the same group
 * of value bindings) is required to implement this.  Where it is known that
 * no overloaded values are defined in a binding (i.e., when the `dreaded
 * monomorphism restriction' strikes), the list used to record dependents
 * is flagged with a NODEPENDS tag to avoid gathering dependents at that
 * level.
 *
 * To interleave between vars for bindings and vars for patterns, we use
 * a list of lists of typing statements for each.  These lists are always
 * the same length.  The implementation here is very similar to that of the
 * dependency analysis used in the static analysis component of this system.
 *
 * To deal with polymorphic recursion, variables defined in bindings can be
 * assigned types of the form (POLYREC,(def,use)), where def is a type
 * variable for the type of the defining occurence, and use is a type
 * scheme for (recursive) calls/uses of the variable.  Because of the the
 * current treatment of type classes, this form of binding should only be
 * used if the declared type is polymorphic, without any class constraints.
 * (It may be possible to weaken this a little in future, by forcing
 * a, potentially less general type scheme that is monomorphic in
 * constrained type variables, but polymorphic in other type vars.)
 * ------------------------------------------------------------------------*/

static List defnBounds;			/*::[[(Var,Type)]] possibly ovrlded*/
static List varsBounds;			/*::[[(Var,Type)]] not overloaded  */
static List depends;			/*::[?[Var]] dependents/NODEPENDS  */

#define saveVarsAssump() List saveAssump = hd(varsBounds)
#define restoreVarsAss() hd(varsBounds)  = saveAssump

static Void local emptyAssumption() {  	/* set empty type assumption	   */
    defnBounds = NIL;
    varsBounds = NIL;
    depends    = NIL;
}

static Void local enterBindings() {    /* Add new level to assumption sets */
    defnBounds = cons(NIL,defnBounds);
    varsBounds = cons(NIL,varsBounds);
    depends    = cons(NIL,depends);
}

static Void local leaveBindings() {    /* Drop one level of assumptions    */
    defnBounds = tl(defnBounds);
    varsBounds = tl(varsBounds);
    depends    = tl(depends);
}

static Int local defType(a)		/* Return type for defining occ.   */
Cell a; {				/* of a var from assumption pair  */
    return (isPair(a) && fst(a)==POLYREC) ? fst(snd(a)) : a;
}

static Type local useType(a)		/* Return type for use of a var	   */
Cell a; {				/* defined in an assumption	   */
    return (isPair(a) && fst(a)==POLYREC) ? snd(snd(a)) : a;
}

static Void local markAssumList(as)	/* Mark all types in assumption set*/
List as; {				/* :: [(Var, Type)]		   */
    for (; nonNull(as); as=tl(as)) {	/* No need to mark generic types;  */
	Type t = defType(snd(hd(as)));	/* the only free variables in those*/
	if (!isPolyType(t))		/* must have been free earlier too */
	    markType(t,0);
    }
}

static Cell local findAssum(t)	       /* Find most recent assumption about*/
Text t; {			       /* variable named t, if any	   */
    List defnBounds1 = defnBounds;     /* return translated variable, with */
    List varsBounds1 = varsBounds;     /* type in typeIs		   */
    List depends1    = depends;

    while (nonNull(defnBounds1)) {
	Pair ass = findInAssumList(t,hd(varsBounds1));/* search varsBounds */
	if (nonNull(ass)) {
	    typeIs = snd(ass);
	    return fst(ass);
	}

	ass = findInAssumList(t,hd(defnBounds1));     /* search defnBounds */
	if (nonNull(ass)) {
	    Cell v = fst(ass);
            typeIs = useType(snd(ass));

	    if (hd(depends1)!=NODEPENDS &&	      /* save dependent?   */
		  isNull(v=varIsMember(t,hd(depends1))))
		/* N.B. make new copy of variable and store this on list of*/
		/* dependents, and in the assumption so that all uses of   */
		/* the variable will be at the same node, if we need to    */
		/* overwrite the call of a function with a translation...  */
		hd(depends1) = cons(v=mkVar(t),hd(depends1));

	    return v;
	}

	defnBounds1 = tl(defnBounds1);		      /* look in next level*/
	varsBounds1 = tl(varsBounds1);		      /* of assumption set */
	depends1    = tl(depends1);
    }
    return NIL;
}

static Pair local findInAssumList(t,as)/* Search for assumption for var    */
Text t;				       /* named t in list of assumptions as*/
List as; {
    for (; nonNull(as); as=tl(as))
	if (textOf(fst(hd(as)))==t)
	    return hd(as);
    return NIL;
}

static List local intsIntersect(as,bs)	/* calculate intersection of lists */
List as, bs; {				/* of integers (as sets)	   */
    List ts = NIL;			/* destructively modifies as	   */
    while (nonNull(as))
	if (intIsMember(intOf(hd(as)),bs)) {
	    List temp = tl(as);
	    tl(as)    = ts;
	    ts	      = as;
	    as	      = temp;
        }
	else
	    as = tl(as);
    return ts;
}

static List local genvarAllAss(as)	/* calculate generic vars that are */
List as; {				/* in every type in assumptions as */
    List vs = genvarTyvar(intOf(defType(snd(hd(as)))),NIL);
    for (as=tl(as); nonNull(as) && nonNull(vs); as=tl(as))
	vs = intsIntersect(vs,genvarTyvar(intOf(defType(snd(hd(as)))),NIL));
    return vs;
}

static List local genvarAnyAss(as)	/* calculate generic vars that are */
List as; {				/* in any type in assumptions as   */
    List vs = genvarTyvar(intOf(defType(snd(hd(as)))),NIL);
    for (as=tl(as); nonNull(as); as=tl(as))
	vs = genvarTyvar(intOf(defType(snd(hd(as)))),vs);
    return vs;
}

#define findTopBinding(v)  findInAssumList(textOf(v),hd(defnBounds))

static Int local newVarsBind(v)        /* make new assump for pattern var  */
Cell v; {
    Int beta	   = newTyvars(1);
    hd(varsBounds) = cons(pair(v,mkInt(beta)), hd(varsBounds));
#ifdef DEBUG_TYPES
    printf("variable, assume ");
    printExp(stdout,v);
    printf(" :: _%d\n",beta);
#endif
    return beta;
}

static Void local newDefnBind(v,type)  /* make new assump for defn var	   */
Cell v; 			       /* and set type if given (nonNull)  */
Type type; {
    Int  beta	   = newTyvars(1);
    Cell ta        = mkInt(beta);
    instantiate(type);
    if (nonNull(type) && isNull(predsAre) && isPolyType(type))
	ta = pair(POLYREC,pair(ta,type));
    hd(defnBounds) = cons(pair(v,ta), hd(defnBounds));
#ifdef DEBUG_TYPES
    printf("definition, assume ");
    printExp(stdout,v);
    printf(" :: _%d\n",beta);
#endif
    bindTv(beta,typeIs,typeOff);       /* Bind beta to new type skeleton   */
}

/* --------------------------------------------------------------------------
 * Predicate sets:
 *
 * A predicate set is represented by a list of triples (C t, o, used)
 * which indicates that type (t,o) must be an instance of class C, with
 * evidence required at the node pointed to by used.  Note that the `used'
 * node may need to be overwritten at a later stage if this evidence is
 * to be derived from some other predicates by entailment.
 * ------------------------------------------------------------------------*/

#include "preds.c"

/* --------------------------------------------------------------------------
 * Type errors:
 * ------------------------------------------------------------------------*/

static Void local typeError(l,e,in,wh,t,o)
Int    l;			      /* line number near type error	   */
String wh;			      /* place in which error occurs	   */
Cell   e;			      /* source of error		   */
Cell   in;			      /* context if any (NIL if not)	   */
Type   t;			      /* should be of type (t,o)	   */
Int    o; {			      /* type inferred is (typeIs,typeOff) */

    clearMarks();		      /* types printed here are monotypes  */
				      /* use marking to give sensible names*/
#ifdef DEBUG_KINDS
{ List vs = genericVars;
  for (; nonNull(vs); vs=tl(vs)) {
     Int v = intOf(hd(vs));
     printf("%c :: ", ('a'+tyvar(v)->offs));
     printKind(stdout,tyvar(v)->kind);
     putchar('\n');
  }
}
#endif

    reportTypeError(l,e,in,wh,copyType(typeIs,typeOff),copyType(t,o));
}

static Void local reportTypeError(l,e,in,wh,inft,expt)
Int    l;				/* error printing part of typeError*/
Cell   e, in;				/* separated out for the benefit of*/
String wh;				/* typing runST			   */
Type   inft, expt; {
    ERRMSG(l)   "Type error in %s", wh    ETHEN
    if (nonNull(in)) {
	ERRTEXT "\n*** expression     : " ETHEN ERREXPR(in);
    }
    ERRTEXT     "\n*** term           : " ETHEN ERREXPR(e);
    ERRTEXT     "\n*** type           : " ETHEN ERRTYPE(inft);
    ERRTEXT     "\n*** does not match : " ETHEN ERRTYPE(expt);
    if (unifyFails) {
	ERRTEXT "\n*** because        : %s", unifyFails ETHEN
    }
    ERRTEXT "\n"
    EEND;
}

#define shouldBe(l,e,in,where,t,o) if (!unify(typeIs,typeOff,t,o)) \
				       typeError(l,e,in,where,t,o);
#define check(l,e,in,where,t,o)    e=typeExpr(l,e); shouldBe(l,e,in,where,t,o)
#define inferType(t,o)		   typeIs=t; typeOff=o

/* --------------------------------------------------------------------------
 * Typing of expressions:
 * ------------------------------------------------------------------------*/

#define EXPRESSION  0			/* type checking expression	   */
#define NEW_PATTERN 1			/* pattern, introducing new vars   */
#define OLD_PATTERN 2			/* pattern, involving bound vars   */
static int tcMode = EXPRESSION;

#ifdef DEBUG_TYPES
static Cell local mytypeExpr	Args((Int,Cell));
static Cell local typeExpr(l,e)
Int l;
Cell e; {
    static int number = 0;
    Cell retv;
    int  mynumber = number++;
    printf("%d) to check: ",mynumber);
    printExp(stdout,e);
    putchar('\n');
    retv = mytypeExpr(l,e);
    printf("%d) result: ",mynumber);
    printType(stdout,debugType(typeIs,typeOff));
    putchar('\n');
    return retv;
}
static Cell local mytypeExpr(l,e)	/* Determine type of expr/pattern  */
#else
static Cell local typeExpr(l,e)		/* Determine type of expr/pattern  */
#endif
Int  l;
Cell e; {
    static String cond	= "conditional";
    static String list	= "list";
    static String discr = "case discriminant";
    static String aspat = "as (@) pattern";

    switch (whatIs(e)) {

	/* The following cases can occur in either pattern or expr. mode   */

	case AP 	: return typeAp(l,e);

	case NAME	: if (isNull(name(e).type))
			      internal("typeExpr1");
			  else {
			      Cell tt = varIntro(e,name(e).type);
			      return isCfun(e) ? e : tt;
			  }

	case TUPLE	: typeTuple(e);
			  break;

#if BIGNUMS
	case POSNUM	:
	case ZERONUM	:
	case NEGNUM	: {   Int alpha = newTyvars(1);
			      inferType(var,alpha);
			      return ap(ap(nameFromInteger,
					   assumeEvid(predNum,alpha)),
					   e);
			  }
#endif
	case INTCELL	: {   Int alpha = newTyvars(1);
			      inferType(var,alpha);
			      return ap(ap(nameFromInt,
					   assumeEvid(predNum,alpha)),
					   e);
			  }

	case FLOATCELL	: {   Int alpha = newTyvars(1);
			      inferType(var,alpha);
			      return ap(ap(nameFromDouble,
					   assumeEvid(predFractional,alpha)),
					   e);
			  }

	case STRCELL	: inferType(typeString,0);
			  break;

	case CHARCELL	: inferType(typeChar,0);
			  break;

	case VAROPCELL	:
	case VARIDCELL	: if (tcMode!=NEW_PATTERN) {
			      Cell a = findAssum(textOf(e));
			      if (nonNull(a))
				  return varIntro(a,typeIs);
			      else {
				   a = findName(textOf(e));
				   if (isNull(a) || isNull(name(a).type))
				       internal("typeExpr2");
				   return varIntro(a,name(a).type);
			      }
			  }
			  else {
			      inferType(var,newVarsBind(e));
			  }
			  break;

	case CONFLDS	: typeConFlds(l,e);
			  break;

	/* The following cases can only occur in expr mode		   */

	case UPDFLDS	: typeUpdFlds(l,e);
			  break;

	case COND	: {   Int beta = newTyvars(1);
			      check(l,fst3(snd(e)),e,cond,typeBool,0);
			      check(l,snd3(snd(e)),e,cond,var,beta);
			      check(l,thd3(snd(e)),e,cond,var,beta);
			      tyvarType(beta);
			  }
			  break;

	case LETREC	: enterBindings();
			  mapProc(typeBindings,fst(snd(e)));
			  snd(snd(e)) = typeExpr(l,snd(snd(e)));
			  leaveBindings();
			  break;

	case FINLIST	: {   Int  beta = newTyvars(1);
			      List xs;
			      for (xs=snd(e); nonNull(xs); xs=tl(xs)) {
				 check(l,hd(xs),e,list,var,beta);
			      }
			      inferType(listof,beta);
			  }
			  break;

	case DOCOMP	: typeDo(l,e);
			  break;

	case COMP	: {   Int beta = newTyvars(1);
			      typeComp(l,listof,snd(e),snd(snd(e)));
			      bindTv(beta,typeIs,typeOff);
			      inferType(listof,beta);
			  }
			  break;

#if LAZY_ST
	case RUNST	: typeRunST(l,e);
			  break;
#endif

	case ESIGN	: typeEsign(l,e);
			  return fst(snd(e));

	case CASE	: {    Int beta = newTyvars(2);    /* discr result */
			       check(l,fst(snd(e)),NIL,discr,var,beta);
			       map2Proc(typeCase,l,beta,snd(snd(e)));
			       tyvarType(beta+1);
			  }
			  break;

	case LAMBDA	: typeAlt(snd(e));
			  break;

	/* The remaining cases can only occur in pattern mode: */

	case WILDCARD	: inferType(var,newTyvars(1));
			  break;

	case ASPAT	: {   Int beta = newTyvars(1);
			      snd(snd(e)) = typeExpr(l,snd(snd(e)));
			      bindTv(beta,typeIs,typeOff);
			      check(l,fst(snd(e)),e,aspat,var,beta);
			      tyvarType(beta);
			  }
			  break;

	case LAZYPAT	: snd(e) = typeExpr(l,snd(e));
			  break;

#if NPLUSK
	case ADDPAT	: {   Int alpha = newTyvars(1);
			      inferType(typeVarToVar,alpha);
			      return ap(e,assumeEvid(predIntegral,alpha));
			  }
#endif

	default 	: internal("typeExpr3");
   }

   return e;
}

static Void local instantiate(type)    /* instantiate type expr, if nonNull*/
Type type; {
    predsAre = NIL;
    typeIs   = type;
    typeOff  = 0;

    if (nonNull(typeIs)) {	       /* instantiate type expression ?    */

	if (isPolyType(typeIs)) {      /* Polymorphic type scheme ?	   */
	    typeOff = newKindedVars(polySigOf(typeIs),tcMode!=EXPRESSION);
	    typeIs  = monoTypeOf(typeIs);
	}

	if (whatIs(typeIs)==QUAL) {    /* Qualified type?		   */
	    predsAre = fst(snd(typeIs));
	    typeIs   = snd(snd(typeIs));
	}
    }
}

static Cell local varIntro(v,type)	/* make translation of var v with  */
Cell v;					/* given type adding any extra dict*/
Type type; {				/* params required		   */
    /* N.B. In practice, v will either be a NAME or a VARID/OPCELL	   */
    for (instantiate(type); nonNull(predsAre); predsAre=tl(predsAre))
	v = ap(v,assumeEvid(hd(predsAre),typeOff));
    return v;
}

#if LAZY_ST
static Void local typeRunST(line,e)	/* Type check encapsulation	   */
Int  line;
Cell e; {
    static String enc = "encapsulation";
    Int    beta       = newTyvars(2);
    List   savePreds  = preds;
    preds             = NIL;
    check(line,snd(e),e,enc,typeSTab,beta);
    clearMarks();
    mapProc(markAssumList,defnBounds);
    mapProc(markAssumList,varsBounds);
    mapProc(markPred,savePreds);
    savePreds = elimConstPreds(line,savePreds);
    if (nonNull(hpreds) && resolveDefs(genvarType(typeSTab,beta,NIL),hpreds))
	savePreds = elimConstPreds(line,savePreds);
    markTyvar(beta+1);
    tyvarType(beta);
    if (nonNull(preds) || typeIs!=var || tyvar(typeOff)->offs==FIXED_TYVAR) {
	Int  alpha  = newTyvars(2);
	Type actual = copyType(typeSTab,beta);
	bindTv(alpha+1,var,beta+1);
	if (nonNull(hpreds))
	    actual = ap(QUAL,pair(copyPreds(hpreds),actual));
	reportTypeError(line,snd(e),e,enc,actual,copyType(typeSTab,alpha));
    }
    preds = savePreds;
    tyvarType(beta+1);
}
#endif

static Void local typeEsign(l,e)	/* Type check expression type sig  */
Int  l;
Cell e; {
    static String typeSig = "type signature expression";
    List savePreds = preds;
    Int  alpha 	   = newTyvars(1);
    Type nt;				/* complete infered type	   */

    instantiate(snd(snd(e)));
    bindTv(alpha,typeIs,typeOff);
    preds = makeEvidArgs(predsAre,typeOff);

    check(l,fst(snd(e)),NIL,typeSig,var,alpha);

    clearMarks();
    mapProc(markAssumList,defnBounds);
    mapProc(markAssumList,varsBounds);
    mapProc(markPred,savePreds);

    savePreds = elimConstPreds(l,savePreds);
    if (nonNull(hpreds) && resolveDefs(genvarTyvar(alpha,NIL),hpreds))
	savePreds = elimConstPreds(l,savePreds);
    resetGenericsFrom(0);
    nt = copyTyvar(alpha);		/* order of copying is *important* */
    nt = generalize(copyPreds(hpreds),nt);

    if (!checkSchemes(snd(snd(e)),nt))
	tooGeneral(l,fst(snd(e)),snd(snd(e)),nt);

    tyvarType(alpha);
    preds = revOnto(preds,savePreds);
}

static Void local typeCase(l,beta,c)   /* type check case: pat -> rhs	   */
Int  l; 			       /* (case given by c == (pat,rhs))   */
Int  beta;			       /* need:  pat :: (var,beta)	   */
Cell c; {			       /*	 rhs :: (var,beta+1)	   */
    static String casePat  = "case pattern";
    static String caseExpr = "case expression";

    saveVarsAssump();

    fst(c) = typeFreshPat(l,fst(c));
    shouldBe(l,fst(c),NIL,casePat,var,beta);
    snd(c) = typeRhs(snd(c));
    shouldBe(l,rhsExpr(snd(c)),NIL,caseExpr,var,beta+1);

    restoreVarsAss();
}

static Void local typeComp(l,m,e,qs)	/* type check comprehension	   */
Int  l;
Type m;					/* monad (mkOffset(0))		   */
Cell e;
List qs; {
    static String boolQual = "boolean qualifier";
    static String genQual  = "generator";

    if (isNull(qs))			/* no qualifiers left		   */
	fst(e) = typeExpr(l,fst(e));
    else {
	Cell q   = hd(qs);
	List qs1 = tl(qs);
	switch (whatIs(q)) {
	    case BOOLQUAL : check(l,snd(q),NIL,boolQual,typeBool,0);
			    typeComp(l,m,e,qs1);
			    break;

	    case QWHERE   : enterBindings();
			    mapProc(typeBindings,snd(q));
			    typeComp(l,m,e,qs1);
			    leaveBindings();
			    break;

	    case FROMQUAL : {   Int beta = newTyvars(1);
				saveVarsAssump();
				check(l,snd(snd(q)),NIL,genQual,m,beta);
				fst(snd(q)) = typeFreshPat(l,fst(snd(q)));
				shouldBe(l,fst(snd(q)),NIL,genQual,var,beta);
				typeComp(l,m,e,qs1);
				restoreVarsAss();
			    }
			    break;

	    case DOQUAL   : check(l,snd(q),NIL,genQual,m,newTyvars(1));
			    typeComp(l,m,e,qs1);
			    break;
	}
    }
}

static Void local typeDo(l,e)		/* type check do-notation	   */
Int  l;
Cell e; {
    static String finGen = "final generator";
    Int  alpha		 = newTyvars(1);
    Int  beta		 = newTyvars(1);
    Cell mon		 = ap(mkInt(beta),var);
    Cell m		 = assumeEvid(predMonad,beta);
    tyvar(beta)->kind    = starToStar;

    typeComp(l,mon,snd(e),snd(snd(e)));
    shouldBe(l,fst(snd(e)),NIL,finGen,mon,alpha);
    snd(e) = pair(pair(m,compZero(snd(snd(e)),beta)),snd(e));
}

static Cell local compZero(qs,beta)	/* return evidence for Monad0 beta */
List qs;				/* if needed for qualifiers qs	   */
Int  beta; {
    for (; nonNull(qs); qs=tl(qs))
	switch (whatIs(hd(qs))) {
	    case FROMQUAL : if (failFree(fst(snd(hd(qs)))))
				break;
			    /* intentional fall-thru */
	    case BOOLQUAL : return assumeEvid(predMonad0,beta);
	}
    return NIL;
}

static Void local typeConFlds(line,e)	/* Type check a construction	   */
Int  line;
Cell e; {
    static String conExpr = "value construction";
    Name c  = fst(snd(e));
    List fs = snd(snd(e));
    Type tc;
    Int  to;
    Int  i;

    instantiate(name(c).type);
    for (; nonNull(predsAre); predsAre=tl(predsAre))
	assumeEvid(hd(predsAre),typeOff);
    tc = typeIs;
    to = typeOff;

    for (; nonNull(fs); fs=tl(fs)) {
	Type t = tc;
	for (i=sfunPos(fst(hd(fs)),c); --i>0; t=arg(t))
	    ;
	check(line,snd(hd(fs)),e,conExpr,arg(fun(t)),to);
    }
    for (i=name(c).arity; i>0; i--)
	tc = arg(tc);
    inferType(tc,to);
}

static Void local typeUpdFlds(line,e)	/* Type check an update		   */
Int  line;				/* (Written in what might seem a   */
Cell e; {				/* bizarre manner for the benefit  */
    static String update = "update";	/* of as yet unreleased extensions)*/
    List cs    = snd3(snd(e));		/* List of constructors		   */
    List fs    = thd3(snd(e));		/* List of field specifications	   */
    List ts    = NIL;			/* List of types for fields	   */
    Int  n     = length(fs);
    Int  alpha = newTyvars(2+n);
    Int  i;
    List fs1;

    /* Calculate type and translation for each expr in the field list	   */
    for (fs1=fs, i=alpha+2; nonNull(fs1); fs1=tl(fs1), i++) {
	snd(hd(fs1)) = typeExpr(line,snd(hd(fs1)));
	bindTv(i,typeIs,typeOff);
    }

    clearMarks();
    mapProc(markAssumList,defnBounds);
    mapProc(markAssumList,varsBounds);
    mapProc(markPred,preds);

    for (fs1=fs, i=alpha+2; nonNull(fs1); fs1=tl(fs1), i++) {
	resetGenericsFrom(0);
	ts = cons(generalize(NIL,copyTyvar(i)),ts);
    }
    ts = rev(ts);

    /* Type check expression to be updated				   */
    fst3(snd(e)) = typeExpr(line,fst3(snd(e)));
    bindTv(alpha,typeIs,typeOff);

    for (; nonNull(cs); cs=tl(cs)) {	/* Loop through constrs		   */
	Name c  = hd(cs);
	List ta = copy(name(c).arity,NIL);
	Type td, tr;
	Int  od, or;

	tcMode = NEW_PATTERN;		/* Domain type			   */
	instantiate(name(c).type);
	tcMode = EXPRESSION;
	td     = typeIs;
	od     = typeOff;
	for (; nonNull(predsAre); predsAre=tl(predsAre))
	    assumeEvid(hd(predsAre),typeOff);

	instantiate(name(c).type);	/* Range type			   */
	tr = typeIs;
	or = typeOff;
	for (; nonNull(predsAre); predsAre=tl(predsAre))
	    assumeEvid(hd(predsAre),typeOff);

	for (fs1=fs, i=1; nonNull(fs1); fs1=tl(fs1), i++) {
	    Int n    = sfunPos(fst(hd(fs1)),c);
	    Cell ta1 = ta;
	    for (; n>1; n--)
		ta1 = tl(ta1);
	    hd(ta1) = mkInt(i);
	}

	for (; nonNull(ta); ta=tl(ta)) {	/* For each cfun arg	   */
	    if (nonNull(hd(ta))) {		/* Field to updated?	   */
		Int  n = intOf(hd(ta));
		Cell f = fs;
		Cell t = ts;
		for (; n-- > 1; f=tl(f), t=tl(t))
		    ;
		f = hd(f);
		t = hd(t);
		instantiate(t);
		shouldBe(line,snd(f),e,update,arg(fun(tr)),or);
	    }					/* Unmentioned component   */
	    else if (!unify(arg(fun(td)),od,arg(fun(tr)),or))
		internal("typeUpdFlds");

	    tr = arg(tr);
	    td = arg(td);
	}

	inferType(td,od);			/* Check domain type	   */
	shouldBe(line,fst3(snd(e)),e,update,var,alpha);
	inferType(tr,or);			/* Check range type	   */
	shouldBe(line,e,NIL,update,var,alpha+1);
    }
    /* (typeIs,typeOff) still carry the result type when we exit the loop  */
}

static Cell local typeFreshPat(l,p)    /* find type of pattern, assigning  */
Int  l; 			       /* fresh type variables to each var */
Cell p; {			       /* bound in the pattern		   */
    tcMode = NEW_PATTERN;
    p	   = typeExpr(l,p);
    tcMode = EXPRESSION;
    return p;
}

/* --------------------------------------------------------------------------
 * Note the pleasing duality in the typing of application and abstraction:-)
 * ------------------------------------------------------------------------*/

static Cell local typeAp(l,e)		/* Type check application	   */
Int  l;
Cell e; {
    static String app = "application";
    Cell h    = getHead(e);		/* e = h e1 e2 ... en		   */
    Int  n    = argCount;		/* save no. of arguments	   */
    Int  beta = funcType(n);
    Cell p    = NIL;			/* points to previous AP node	   */
    Cell a    = e;			/* points to current AP node	   */
    Int  i;

    check(l,h,e,app,var,beta);		/* check h::t1->t2->...->tn->rn+1  */
    for (i=n; i>0; --i) {		/* check e_i::t_i for each i	   */
	check(l,arg(a),e,app,var,beta+2*i-1);
	p = a;
	a = fun(a);
    }
    fun(p) = h;				/* replace head with translation   */
    tyvarType(beta+2*n);		/* inferred type is r_n+1	   */
    return e;
}

static Void local typeAlt(a)		/* Type check abstraction (Alt)	   */
Cell a; {				/* a = ( [p1, ..., pn], rhs )	   */
    List ps	  = fst(a);
    Int  n	  = length(ps);
    Int  beta	  = funcType(n);
    Int  l	  = rhsLine(snd(a));
    Int  i;

    saveVarsAssump();

    for (i=0; i<n; ++i) {
	hd(ps) = typeFreshPat(l,hd(ps));
	bindTv(beta+2*i+1,typeIs,typeOff);
	ps = tl(ps);
    }
    snd(a) = typeRhs(snd(a));
    bindTv(beta+2*n,typeIs,typeOff);
    tyvarType(beta);

    restoreVarsAss();
}

static Int local funcType(n)		/*return skeleton for function type*/
Int n; {				/*with n arguments, taking the form*/
    Int beta = newTyvars(2*n+1);	/*    r1 t1 r2 t2 ... rn tn rn+1   */
    Int i;				/* with r_i := t_i -> r_i+1	   */
    for (i=0; i<n; ++i)
	bindTv(beta+2*i,arrow,beta+2*i+1);
    return beta;
}

/* --------------------------------------------------------------------------
 * Tuple type constructors: are generated as necessary.  The most common
 * n-tuple constructors (n<MAXTUPCON) are held in a cache to avoid
 * repeated generation of the constructor types.
 * ------------------------------------------------------------------------*/

#define MAXTUPCON 10
static Type tupleConTypes[MAXTUPCON];

static Void local typeTuple(e)	       /* find type for tuple constr, using*/
Cell e; {			       /* tupleConTypes to cache previously*/
    Int n   = tupleOf(e);	       /* calculated tuple constr. types.  */
    typeOff = newTyvars(n);
    if (n>=MAXTUPCON)
	 typeIs = makeTupleType(n);
    else if (tupleConTypes[n])
	 typeIs = tupleConTypes[n];
    else
	 typeIs = tupleConTypes[n] = makeTupleType(n);
}

static Type local makeTupleType(n)     /* construct type for tuple constr. */
Int n; {			       /* t1 -> ... -> tn -> (t1,...,tn)   */
    Type h = mkTuple(n);
    Int  i;

    for (i=0; i<n; ++i)
	h = ap(h,mkOffset(i));
    while (0<n--)
	h = fn(mkOffset(n),h);
    return h;
}

/* --------------------------------------------------------------------------
 * Type check group of bindings:
 * ------------------------------------------------------------------------*/

static Void local typeBindings(bs)	/* type check a binding group	   */
List bs; {
    Bool usesPatBindings = FALSE;	/* TRUE => pattern binding in bs   */
    Bool usesUntypedVar  = FALSE;	/* TRUE => var bind w/o type decl  */
    List bs1;

    /* The following loop is used to determine whether the monomorphism	   */
    /* restriction should be applied.  It could be written marginally more */
    /* efficiently by using breaks, but clarity is more important here ... */

    for (bs1=bs; nonNull(bs1); bs1=tl(bs1)) {  /* Analyse binding group    */
	Cell b = hd(bs1);
	if (!isVar(fst(b)))
	    usesPatBindings = TRUE;
	else if (isNull(fst(hd(snd(snd(b))))) && isNull(fst(snd(b))))
	    usesUntypedVar  = TRUE;
    }

    hd(defnBounds) = NIL;
    hd(depends)	   = NIL;

    if (usesPatBindings || usesUntypedVar)
	monorestrict(bs);
    else
	unrestricted(bs);

    mapProc(removeTypeSigs,bs);		       /* Remove binding type info */
    hd(varsBounds) = revOnto(hd(defnBounds),   /* transfer completed assmps*/
			     hd(varsBounds));  /* out of defnBounds        */
    hd(defnBounds) = NIL;
    hd(depends)    = NIL;
}

static Void local removeTypeSigs(b)    /* Remove type info from a binding  */
Cell b; {
    snd(b) = snd(snd(b));
}

/* --------------------------------------------------------------------------
 * Type check a restricted binding group:
 * ------------------------------------------------------------------------*/

static Void local monorestrict(bs)	/* Type restricted binding group   */
List bs; {
    List savePreds = preds;
    Int  line 	   = isVar(fst(hd(bs))) ? rhsLine(snd(hd(snd(snd(hd(bs))))))
					: rhsLine(snd(snd(snd(hd(bs)))));

    hd(depends) = NODEPENDS;	       /* No need for dependents here	   */
    preds       = NIL;

    mapProc(restrictedBindAss,bs);     /* add assumptions for vars in bs   */
    mapProc(typeBind,bs);	       /* type check each binding	   */

    clearMarks();		       /* mark fixed variables		   */
    mapProc(markAssumList,tl(defnBounds));
    mapProc(markAssumList,tl(varsBounds));
    mapProc(markPred,savePreds);
    if (nonNull(tl(defnBounds)))
	mapProc(markPred,preds);

    savePreds = elimConstPreds(line,savePreds);

    if (isNull(tl(defnBounds))) {	/* top-level may need defaulting   */
	if (nonNull(hpreds) &&
	    resolveDefs(genvarAnyAss(hd(defnBounds)),hpreds))
	    savePreds = elimConstPreds(line,savePreds);
	if (nonNull(preds)) {		/* look for unresolved overloading */
	    Cell v   = isVar(fst(hd(bs))) ? fst(hd(bs)) : hd(fst(hd(bs)));
	    Cell ass = findInAssumList(textOf(v),hd(varsBounds));

	    ERRMSG(line) "Unresolved top-level overloading" ETHEN
	    ERRTEXT     "\n*** Binding             : %s", textToStr(textOf(v))
	    ETHEN
	    if (nonNull(ass)) {
		ERRTEXT "\n*** Inferred type       : " ETHEN ERRTYPE(snd(ass));
	    }
	    ERRTEXT     "\n*** Outstanding context : " ETHEN
						ERRCONTEXT(copyPreds(hpreds));
	    ERRTEXT     "\n"
	    EEND;
	}
    }
    preds     = appendOnto(preds,savePreds);
    hpreds    = NIL;

    map2Proc(genBind,NIL,NIL,bs);	/* Generalize types of def'd vars  */
}

static Void local restrictedBindAss(b) /* make assums for vars in binding  */
Cell b; {			       /* gp with restricted overloading   */

    if (isVar(fst(b)))		       /* function-binding?		   */
	restrictedAss(rhsLine(snd(hd(snd(snd(b))))),
		      fst(b),
		      fst(snd(b)));
    else {			       /* pattern-binding?		   */
	List vs   = fst(b);
	List ts   = fst(snd(b));
	Int  line = rhsLine(snd(snd(snd(b))));

	for (; nonNull(vs); vs=tl(vs))
	    if (nonNull(ts)) {
		restrictedAss(line,hd(vs),hd(ts));
		ts = tl(ts);
	    }
	    else
		restrictedAss(line,hd(vs),NIL);
    }
}

static Void local restrictedAss(l,v,t) /* Assume that type of binding var v*/
Int  l; 			       /* is t (if nonNull) in restricted  */
Cell v; 			       /* binding group 		   */
Type t; {
    newDefnBind(v,t);
    if (nonNull(predsAre)) {
	ERRMSG(l) "Explicit overloaded type for \"%s\"",textToStr(textOf(v))
	ETHEN
	ERRTEXT   " not permitted in restricted binding"
	EEND;
    }
}

/* --------------------------------------------------------------------------
 * Unrestricted binding group:
 * ------------------------------------------------------------------------*/

static Void local unrestricted(bs)	/* Type unrestricted binding group */
List bs; {
    Int  line      = rhsLine(snd(hd(snd(snd(hd(bs))))));
    List savePreds = preds;
    List bs1;

    preds = NIL;

    for (bs1=bs; nonNull(bs1); bs1=tl(bs1)) {   /* Add assumptions about   */
	Cell b = hd(bs1);			/* each bound var -- can   */
	newDefnBind(fst(b),fst(snd(b)));	/* assume function binding */
	for (; nonNull(predsAre); predsAre=tl(predsAre))
	    assumeEvid(hd(predsAre),typeOff);
    }

    mapProc(typeBind,bs);			/* type check each binding */

    clearMarks();				/* Mark fixed variables	   */
    mapProc(markAssumList,tl(defnBounds));
    mapProc(markAssumList,tl(varsBounds));
    mapProc(markPred,savePreds);

    savePreds = elimConstPreds(line,savePreds);
    if (nonNull(hpreds) && resolveDefs(genvarAllAss(hd(defnBounds)),hpreds))
	savePreds = elimConstPreds(line,savePreds);

    map2Proc(genBind,preds,hpreds,bs);	/* Generalize types of def'd vars  */

    if (nonNull(preds)) {		/* Add dictionary params, if nec.  */
	map1Proc(addEvidParams,preds,hd(depends));
	map1Proc(qualifyBinding,preds,bs);
    }

    preds  = savePreds;			/* restore predicates		   */
    hpreds = NIL;
}

static Void local addEvidParams(qs,v)  /* overwrite VARID/OPCELL v with	   */
List qs;			       /* application of variable to evid. */
Cell v; {			       /* parameters given by qs	   */
    if (nonNull(qs)) {
	Cell nv;

	if (!isVar(v))
	    internal("addEvidParams");

	for (nv=mkVar(textOf(v)); nonNull(tl(qs)); qs=tl(qs))
	    nv = ap(nv,thd3(hd(qs)));
	fst(v) = nv;
	snd(v) = thd3(hd(qs));
    }
}

/* --------------------------------------------------------------------------
 * Type check bodies of class and instance declarations:
 * ------------------------------------------------------------------------*/

static Void local typeInstDefn(in)	/* type check implementations of   */
Inst in; {				/* member functions for instance in*/
    Int i;
    Cell head = inst(in).t;
    List sig  = NIL;
    List k    = kindAtom(inst(in).t);

    for (i=0; i<inst(in).arity; ++i)
	head = ap(head,mkOffset(i));
    for (i=0; i<inst(in).arity; ++i, k=snd(k))
	sig  = cons(fst(k),sig);
    sig = rev(sig);

    typeMembers("instance member binding",
		cclass(inst(in).c).members,
		inst(in).implements,
		inst(in).specifics,
		dictSpecificsStart(inst(in).c),
		sig,
		head);
}

static Void local typeClassDefn(c)	/* type check implementations of   */
Class c; {				/* defaults for class c		   */
    typeMembers("default member binding",
		cclass(c).members,
		cclass(c).defaults,
		singleton(ap(c,var)),
		0,
		singleton(cclass(c).sig),
		var);
}

static Void local typeMembers(wh,ms,is,specifics,spoff,sig,head)
String wh;				/* type check implementations `is' */
List   ms;				/* of members `ms' for a specific  */
List   is;				/* class instance		   */
List   specifics;
Int    spoff;
List   sig;
Type   head; {
    while (nonNull(is)) {
	if (isName(hd(is)))
	    typeMember(wh,hd(ms),hd(is),specifics,spoff,sig,head);
	is = tl(is);
	ms = tl(ms);
    }
}

static Void local typeMember(wh,m,i,specifics,spoff,sig,head)
String wh;				/* type check implementation i of  */
Name   m;				/* member m for instance type head */
Name   i;				/* (with kinds given by sig) and   */
List   specifics;			/* using the given specifics	   */
Int    spoff;				/* at the specified offset spoff   */
List   sig;
Type   head; {
    Int  line = rhsLine(snd(hd(name(i).defn)));
    Int  alpha, beta;
    Type required, inferred;
    List extras;

#ifdef DEBUG_TYPES
    printf("Line %d, instance type: ",line);
    printType(stdout,head);
    putchar('\n');
#endif

    emptySubstitution();
    hd(defnBounds) = NIL;
    hd(depends)    = NODEPENDS;
    preds          = NIL;

    alpha    = newTyvars(1);		/* Set required instance of m	   */
    beta     = newKindedVars(sig,FALSE);
    instantiate(name(m).type);
    bindTv(alpha,typeIs,typeOff);
    bindTv(typeOff,head,beta);
    extras   = makeEvidArgs(tl(predsAre),typeOff);
    required = copyTyvar(alpha);

#ifdef DEBUG_TYPES
    printf("Checking implementation of: ");
    printExp(stdout,m);
    printf(" :: ");
    printType(stdout,required);
    printf("\n");
#endif

    map2Proc(typeDefAlt,alpha,m,name(i).defn);

    if (nonNull(extras)) {		/* Now deal with predicates ...    */
	List ps = NIL;
	while (nonNull(preds)) {        /* discharge preds entailed by	   */
	    List nx = tl(preds);	/* the `extras'			   */
	    Cell pi = hd(preds);
	    Cell ev = simpleEntails(extras,fst3(pi),intOf(snd3(pi)));
	    if (nonNull(ev))
		overEvid(thd3(pi),ev);
	    else {
		tl(preds) = ps;
		ps        = preds;
	    }
	    preds = nx;
	}
	preds = rev(ps);
	map1Proc(qualify,extras,name(i).defn);
    }

    clearMarks();
    if (nonNull(elimConstPreds(line,NIL)) ||	/* discharge const preds   */
	(resolveDefs(genvarTyvar(alpha,NIL),hpreds) &&
	 nonNull(elimConstPreds(line,NIL))))
	internal("typeMember");

    resetGenericsFrom(0);
    inferred = copyTyvar(alpha);	/* Compare with inferred type	   */
    if (!equalTypes(required,inferred))
	tooGeneral(line,m,required,inferred);

#ifdef DEBUG_TYPES
    printf("preds = ");
    printContext(stdout,copyPreds(preds));
    putchar('\n');
    printf("hpreds= ");
    printContext(stdout,copyPreds(hpreds));
    putchar('\n');
#endif

    for (; nonNull(hpreds); hpreds=tl(hpreds)) {
	List ps = specifics;
	Cell pi = hd(hpreds);
	Int  i  = spoff;
	Cell ev = NIL;
	for (; isNull(ev) && nonNull(ps); ps=tl(ps), ++i)
	    if (sameType(arg(fst3(pi)),intOf(snd3(pi)),arg(hd(ps)),beta))
		ev = superEvid(mkOffset(i),fun(hd(ps)),fun(fst3(pi)));
	if (nonNull(ev))
	    overEvid(thd3(pi),ev);
	else {
	    ERRMSG(line) "Insufficient class constraints in %s", wh ETHEN
	    ERRTEXT "\n*** Context  : " ETHEN ERRCONTEXT(specifics);
	    ERRTEXT "\n*** Required : " ETHEN
	    ERRPRED(copyPred(fst3(pi),intOf(snd3(pi))));
	    ERRTEXT "\n"
	    EEND;
	}
    }

    mapOver(tidyEvid,evids);			/* avoid unnec. indirects. */

#ifdef DEBUG_TYPES
    printf("evids = "); printExp(stdout,evids); putchar('\n');
#endif

    map1Proc(qualify,preds,name(i).defn);	/* add extra dict params   */
    name(i).type = evids;			/* save evidence	   */
    overDefns    = cons(i,overDefns);		/* add to list of impls.   */
}

/* --------------------------------------------------------------------------
 * Type check bodies of bindings:
 * ------------------------------------------------------------------------*/

static Void local typeBind(b)	       /* Type check binding		   */
Cell b; {
    if (isVar(fst(b))) {			       /* function binding */
	Cell ass = findTopBinding(fst(b));
	Int  beta;

	if (isNull(ass))
	    internal("typeBind");

	beta = intOf(defType(snd(ass)));
	map2Proc(typeDefAlt,beta,fst(b),snd(snd(b)));
    }
    else {					       /* pattern binding  */
	static String lhsPat = "lhs pattern";
	static String rhs    = "right hand side";
	Int  beta	     = newTyvars(1);
	Pair pb		     = snd(snd(b));
	Int  l		     = rhsLine(snd(pb));

	tcMode  = OLD_PATTERN;
	check(l,fst(pb),NIL,lhsPat,var,beta);
	tcMode  = EXPRESSION;
	snd(pb) = typeRhs(snd(pb));
	shouldBe(l,rhsExpr(snd(pb)),NIL,rhs,var,beta);
    }
}

static Void local typeDefAlt(beta,v,a) /* type check alt in func. binding  */
Int  beta;
Cell v;
Pair a; {
    static String valDef = "function binding";
    Int l		 = rhsLine(snd(a));
    typeAlt(a);
    shouldBe(l,v,NIL,valDef,var,beta);
}

static Cell local typeRhs(e)	       /* check type of rhs of definition  */
Cell e; {
    switch (whatIs(e)) {
	case GUARDED : {   Int beta = newTyvars(1);
			   map1Proc(guardedType,beta,snd(e));
			   tyvarType(beta);
		       }
		       break;

	case LETREC  : enterBindings();
		       mapProc(typeBindings,fst(snd(e)));
		       snd(snd(e)) = typeRhs(snd(snd(e)));
		       leaveBindings();
		       break;

	default      : snd(e) = typeExpr(intOf(fst(e)),snd(e));
		       break;
    }
    return e;
}

static Void local guardedType(beta,gded)/* check type of guard (li,(gd,ex))*/
Int  beta;			       /* should have gd :: Bool,	   */
Cell gded; {			       /*	      ex :: (var,beta)	   */
    static String guarded = "guarded expression";
    static String guard   = "guard";
    Int line = intOf(fst(gded));

    gded     = snd(gded);
    check(line,fst(gded),NIL,guard,typeBool,0);
    check(line,snd(gded),NIL,guarded,var,beta);
}

Cell rhsExpr(rhs)		       /* find first expression on a rhs   */
Cell rhs; {
    switch (whatIs(rhs)) {
	case GUARDED : return snd(snd(hd(snd(rhs))));
	case LETREC  : return rhsExpr(snd(snd(rhs)));
	default      : return snd(rhs);
    }
}

Int rhsLine(rhs)		       /* find line number associated with */
Cell rhs; {			       /* a right hand side		   */
    switch (whatIs(rhs)) {
	case GUARDED : return intOf(fst(hd(snd(rhs))));
	case LETREC  : return rhsLine(snd(snd(rhs)));
	default      : return intOf(fst(rhs));
    }
}

/* --------------------------------------------------------------------------
 * Calculate generalization of types and compare with declared type schemes:
 * ------------------------------------------------------------------------*/

static Void local genBind(ps,hps,b)	/* Generalize the type of each var */
List ps;				/* defined in binding b, qualifying*/
List hps;				/* each with the predicates in ps  */
Cell b; {				/* and using Haskell predicates hps*/
    Cell v = fst(b);
    Cell t = fst(snd(b));

    if (isVar(fst(b)))
	genAss(rhsLine(snd(hd(snd(snd(b))))),ps,hps,v,t);
    else {
	Int line = rhsLine(snd(snd(snd(b))));
	for (; nonNull(v); v=tl(v)) {
	    Type ty = NIL;
	    if (nonNull(t)) {
		ty = hd(t);
		t  = tl(t);
	    }
	    genAss(line,ps,hps,hd(v),ty);
	}
    }
}

static Void local genAss(l,ps,hps,v,t)	/* Calculate inferred type of v and*/
Int  l;					/* compare with declared type, t,  */
List ps;				/* if given.  Use Haskell preds hps*/
List hps;				/* to check correct unambig typing */
Cell v;					/* and ps to calculate GTC type    */
Type t; {
    Cell ass = findTopBinding(v);
    Type it;
    Int  ng;
    Type ht;

    if (isNull(ass))
	internal("genAss");

    resetGenericsFrom(0);		/* Calculate Haskell typing	   */
    it  = copyTyvar(intOf(defType(snd(ass))));
    ng  = nextGeneric;
    hps = copyPreds(hps);
    ht  = generalize(hps,it);

    if (nextGeneric!=ng)		/* If a new generic variable was   */
	ambigError(l,			/* introduced by copyHPreds, then  */
		   "inferred type",	/* the inferred type is ambiguous  */
		   v,
		   ht);

    if (nonNull(t) && !checkSchemes(t,ht))
	tooGeneral(l,v,t,ht);		/* Compare with declared type	   */

    snd(ass) = generalize(copyPreds(ps),it);
#ifdef DEBUG_TYPES
    printExp(stdout,v);
    printf(" :: ");
    printType(stdout,snd(ass));
    printf("\n");
#endif
}

static Type local generalize(qs,t)	/* calculate generalization of t   */
List qs;				/* having already marked fixed vars*/
Type t; {				/* with qualifying preds qs	   */
    if (nonNull(qs))
	t = ap(QUAL,pair(qs,t));
    if (nonNull(genericVars)) {
	Kind k  = STAR;
	List vs = genericVars;
	for (; nonNull(vs); vs=tl(vs)) {
	    Tyvar *tyv = tyvar(intOf(hd(vs)));
	    Kind   ka  = tyv->kind;
	    k = ap(ka,k);
	}
	t = mkPolyType(k,t);
#ifdef DEBUG_KINDS
    printf("Generalised type: ");
    printType(stdout,t);
    printf(" ::: ");
    printKind(stdout,k);
    printf("\n");
#endif
    }
    return t;
}

static Void local tooGeneral(l,e,dt,it)	/* explicit type sig. too general  */
Int  l;
Cell e;
Type dt, it; {
    ERRMSG(l) "Declared type too general" ETHEN
    ERRTEXT   "\n*** Expression    : "	  ETHEN ERREXPR(e);
    ERRTEXT   "\n*** Declared type : "	  ETHEN ERRTYPE(dt);
    ERRTEXT   "\n*** Inferred type : "	  ETHEN ERRTYPE(it);
    ERRTEXT   "\n"
    EEND;
}

/* --------------------------------------------------------------------------
 * Compare type schemes:
 *
 * In comparing declared and inferred type schemes, we require that the type
 * parts of the two type schemes are identical.  However, for the predicate
 * parts of the two type schemes, we require only that each inferred
 * predicate is included in the list of declared predicates:
 *
 * e.g. Declared        Inferred
 *      (Eq a, Eq a)    Eq a		OK
 *      (Ord a, Eq a)   Ord a		OK
 *      ()              Ord a		NOT ACCEPTED
 *	Ord a		()		IMPOSSIBLE, by construction, the
 *					inferred context will be at least as
 *					restricted as the declared context.
 * ------------------------------------------------------------------------*/

static Bool local checkSchemes(sd,si)	/* Compare type schemes		   */
Type sd;				/* declared scheme		   */
Type si; {				/* inferred scheme		   */
    Bool bd = isPolyType(sd);
    Bool bi = isPolyType(si);
    if (bd || bi) {
        if (bd && bi && eqKind(polySigOf(sd),polySigOf(si))) {
            sd = monoTypeOf(sd);
            si = monoTypeOf(si);
        }
        else
            return FALSE;
    }

    bd = (whatIs(sd)==QUAL);
    bi = (whatIs(si)==QUAL);
    if (bd && bi && checkQuals(fst(snd(sd)),fst(snd(si)))) {
	sd = snd(snd(sd));
	si = snd(snd(si));
    }
    else if (bd && !bi && isNull(fst(snd(sd))))	/* maybe somebody gave an   */
	sd = snd(snd(sd));			/* explicitly null context? */
    else if (!bd && bi && isNull(fst(snd(si))))
	si = snd(snd(si));
    else if (bd || bi)
	return FALSE;

    return equalTypes(sd,si);
}

static Bool local checkQuals(qsd,qsi)  /* Compare lists of qualifying preds*/
List qsd, qsi; {
    for (; nonNull(qsi); qsi=tl(qsi)) {			/* check qsi < qsd */
	Class c  = fun(hd(qsi));
	Type  o  = arg(hd(qsi));
	List  qs = qsd;
	for (; nonNull(qs); qs=tl(qs))
	   if (c==fun(hd(qs)) && o==arg(hd(qs)))
		break;
	if (isNull(qs))
	    return FALSE;
    }
    return TRUE;
}

static Bool local equalTypes(t1,t2)    /* Compare simple types for equality*/
Type t1, t2; {

et: if (whatIs(t1)!=whatIs(t2))
	return FALSE;

    switch (whatIs(t1)) {
	case TYCON   :
	case OFFSET  :
	case TUPLE   : return t1==t2;

	case INTCELL : return intOf(t1)!=intOf(t2);

	case AP      : if (equalTypes(fun(t1),fun(t2))) {
			   t1 = arg(t1);
			   t2 = arg(t2);
			   goto et;
		       }
                       return FALSE;

	default      : internal("equalTypes");
    }

    return TRUE;/*NOTREACHED*/
}

/* --------------------------------------------------------------------------
 * Entry points to type checker:
 * ------------------------------------------------------------------------*/

Type typeCheckExp(useDefs)		/* Type check top level expression */
Bool useDefs; {				/* using defaults if reqd	   */
    Type type;

    typeChecker(RESET);
    enterBindings();
    inputExpr = typeExpr(0,inputExpr);
    clearMarks();
    if (nonNull(elimConstPreds(0,NIL)) ||
	(useDefs && resolveDefs(NIL,hpreds) &&
	 nonNull(elimConstPreds(0,NIL))))
	internal("typeCheckExp");
    resetGenericsFrom(0);
    type = copyType(typeIs,typeOff);
    type = generalize(copyPreds(hpreds),type);
    if (nonNull(preds)) {		/* qualify input expression with   */
	if (whatIs(inputExpr)!=LAMBDA)	/* additional dictionary params	   */
	    inputExpr = ap(LAMBDA,pair(NIL,pair(mkInt(0),inputExpr)));
	qualify(preds,snd(inputExpr));
    }
    typeChecker(RESET);
    return type;
}

Cell getDictFor(c,t)		       /* Find dictionary (or NIL) for a   */
Class c;			       /* monotype instance of t to be in  */
Type  t; {			       /* the class c			   */
    Cell mt = fullExpand(isPolyType(t) ? monoTypeOf(t) : t);
    Cell d  = NIL;
    if (whatIs(mt)!=QUAL && mtInst(c,mt)) {
	typeChecker(RESET);
	instantiate(t);
	d = makeDictFor(getEvid(0,c,typeIs,typeOff),NIL);
	typeChecker(RESET);
    }
    return d;
}

Void typeCheckDefns() { 	       /* Type check top level bindings    */
    Target t  = length(selDefns)  + length(valDefns) +
		length(instDefns) + length(classDefns);
    Target i  = 0;
    List   gs;

    typeChecker(RESET);
    enterBindings();
    dictsPending = NIL;
    setGoal("Type checking",t);

    for (gs=selDefns; nonNull(gs); gs=tl(gs)) {
	mapProc(typeSel,hd(gs));
	soFar(i++);
    }
    for (gs=valDefns; nonNull(gs); gs=tl(gs)) {
	typeDefnGroup(hd(gs));
	soFar(i++);
    }
    clearTypeIns();
    for (gs=instDefns; nonNull(gs); gs=tl(gs)) {
	typeInstDefn(hd(gs));
	soFar(i++);
    }
    for (gs=classDefns; nonNull(gs); gs=tl(gs)) {
	typeClassDefn(hd(gs));
	soFar(i++);
    }

    makePendingDicts();
    typeChecker(RESET);
    done();
}

static Void local typeDefnGroup(bs)	/* type check group of value defns */
List bs; {				/* (one top level scc)		   */
    List as;

    emptySubstitution();
    hd(defnBounds) = NIL;
    preds	   = NIL;
    setTypeIns(bs);
    typeBindings(bs);			/* find types for vars in bindings */

    if (nonNull(preds))
	internal("typeDefnGroup");

    for (as=hd(varsBounds); nonNull(as); as=tl(as)) {
	Cell a = hd(as);		/* add infered types to environment*/
	Name n = findName(textOf(fst(a)));
	if (isNull(n))
	    internal("typeDefnGroup");
	name(n).type = snd(a);
    }
    hd(varsBounds) = NIL;
}

static Void local typeSel(s)		/* Calculate a suitable type for a */
Name s; {				/* particular selector, s	   */
    List cns  = name(s).defn;
    Type doms = NIL;			/* skeleton and offset for domain  */
    Int  domo;
    Type dom  = NIL;			/* Inferred domain		   */
    Type rng  = NIL;			/* Inferred range		   */
    Type rng1 = NIL;

#ifdef DEBUG_SELS
    printf("Selector %s, cns=",textToStr(name(s).text));
    printExp(stdout,cns);
    putchar('\n');
#endif

    emptySubstitution();
    preds  = NIL;
    tcMode = NEW_PATTERN;

    for (; nonNull(cns); cns=tl(cns)) {
	Name c = fst(hd(cns));
	Int  n = intOf(snd(hd(cns)));
	Int  a = name(c).arity;
	Type rngs;

	instantiate(name(c).type);	/* Instantiate constructor type	   */
	for (; nonNull(predsAre); predsAre=tl(predsAre))
	    assumeEvid(hd(predsAre),typeOff);

	for (; --n>0; a--)		/* Get skeleton for range	   */
	    typeIs = arg(typeIs);
	rngs = arg(fun(typeIs));
	for (; a>0; a--)		/* And then look for domain	   */
	    typeIs = arg(typeIs);

	if (isNull(doms)) {		/* Save first domain type and then */
	    doms = typeIs;		/* unify with subsequent domains to*/
	    domo = typeOff;		/* ensure that any pred sets match */
	}
	else if (!unify(typeIs,typeOff,doms,domo))
	    internal("typeSel");

	clearMarks();			/* Now get uniform representation  */
	dom = copyType(doms,domo);	/* for selector type		   */
	rng = copyType(rngs,typeOff);

	if (isNull(rng1))		/* Compare component types	   */
	    rng1 = fullExpand(rng);
	else if (!equalTypes(rng1,fullExpand(rng))) {
	    ERRMSG(name(s).line) "Mismatch in field types for selector \"%s\"",
				 textToStr(name(s).text) ETHEN
	    ERRTEXT "\n*** Field type     : " 		 ETHEN ERRTYPE(rng1);
	    ERRTEXT "\n*** Does not match : " 		 ETHEN ERRTYPE(rng);
	    ERRTEXT "\n"
	    EEND;
	}
    }
    preds	  = simplify(preds);
    name(s).type  = generalize(copyPreds(preds),fn(dom,rng));
    name(s).arity = 1+length(preds);
    tcMode        = EXPRESSION;
    implementSfun(s);
#ifdef DEBUG_SELS
    printf("Inferred arity = %d, type = ",name(s).arity);
    printType(stdout,name(s).type);
    putchar('\n');
#endif
}

/* --------------------------------------------------------------------------
 * Type checker control:
 * ------------------------------------------------------------------------*/

Void typeChecker(what)
Int what; {
    Int  i;

    switch (what) {
	case RESET   : tcMode	    = EXPRESSION;
		       matchMode    = FALSE;
		       unkindTypes  = NIL;
		       emptySubstitution();
		       emptyAssumption();
		       preds        = NIL;
		       evids	    = NIL;
		       hpreds	    = NIL;
		       dictsPending = NONE;
		       break;

	case MARK    : for (i=0; i<MAXTUPCON; ++i)
			   mark(tupleConTypes[i]);
		       for (i=0; i<MAXKINDFUN; ++i) {
			   mark(simpleKindCache[i]);
			   mark(varKindCache[i]);
		       }
		       for (i=0; i<numTyvars; ++i)
			   mark(tyvars[i].bound);
		       mark(typeIs);
		       mark(predsAre);
		       mark(defnBounds);
		       mark(varsBounds);
		       mark(depends);
		       mark(preds);
		       mark(evids);
		       mark(hpreds);
		       mark(dictsPending);
		       mark(evalDict);
		       mark(stdDefaults);
		       mark(unkindTypes);
		       mark(genericVars);
		       mark(arrow);
		       mark(boundPair);
		       mark(listof);
		       mark(typeVarToVar);
		       mark(predNum);
		       mark(predFractional);
		       mark(predIntegral);
		       mark(starToStar);
		       mark(predMonad);
		       mark(predMonad0);
#if IO_MONAD
		       mark(typeProgIO);
#endif
#if LAZY_ST
		       mark(typeSTab);
#endif
		       break;

	case INSTALL : typeChecker(RESET);

		       for (i=0; i<MAXTUPCON; ++i)
			   tupleConTypes[i] = NIL;
		       for (i=0; i<MAXKINDFUN; ++i) {
			   simpleKindCache[i] = NIL;
			   varKindCache[i]    = NIL;
		       }

		       starToStar   = simpleKind(1);

		       typeUnit     = addPrimTycon("()",STAR,0,DATATYPE,NIL);
		       typeArrow    = addPrimTycon("(->)",simpleKind(2),2,
								DATATYPE,NIL);
		       typeList	    = addPrimTycon("[]",starToStar,1,
								DATATYPE,NIL);

		       var	    = mkOffset(0);
		       arrow	    = fn(var,mkOffset(1));
		       listof	    = ap(typeList,var);
		       boundPair    = ap(ap(mkTuple(2),var),var);

		       nameUnit	    = addPrimCfun(findText("()"),0,0,typeUnit);
		       tycon(typeUnit).defn
				    = singleton(nameUnit);

		       nameNil	    = addPrimCfun(findText("[]"),0,1,
						   mkPolyType(starToStar,
							      listof));
		       nameCons     = addPrimCfun(findText(":"),2,2,
						   mkPolyType(starToStar,
							      fn(var,
							      fn(listof,
								 listof))));
		       tycon(typeList).defn
				    = cons(nameNil,cons(nameCons,NIL));

		       typeVarToVar = fn(var,var);
#if IO_MONAD
		       nameUserErr  = addPrimCfun(inventText(),1,1,NIL);
		       nameIllegal  = addPrimCfun(inventText(),0,2,NIL);
		       nameNameErr  = addPrimCfun(inventText(),1,3,NIL);
		       nameSearchErr= addPrimCfun(inventText(),1,4,NIL);
		       nameWriteErr = addPrimCfun(inventText(),1,5,NIL);
		       nameEvalErr  = addPrimCfun(inventText(),1,6,NIL);
#endif
#if LAZY_ST
		       typeST	    = addPrimTycon("ST",simpleKind(2),2,
						   DATATYPE,NIL);
		       typeSTab	    = ap(ap(typeST,mkOffset(0)),mkOffset(1));
#endif
		       /* Dummy dictionary for (all) instances of Eval has
			* no member, superclass, or specific slots...
			*/
		       evalDict     = newDict(1);
		       break;
    }
}

Void linkPreludeTC() {			/* Hook to tycons and classes in   */
    if (isNull(typeBool)) {		/* prelude when first loaded	   */
	Int i;

	typeBool     = findTycon(findText("Bool"));
	typeChar     = findTycon(findText("Char"));
	typeString   = findTycon(findText("String"));
	typeInt      = findTycon(findText("Int"));
	typeDouble   = findTycon(findText("Double"));
	typeMaybe    = findTycon(findText("Maybe"));
	typeOrdering = findTycon(findText("Ordering"));
	if (isNull(typeBool) || isNull(typeChar)   || isNull(typeString) ||
	    isNull(typeInt)  || isNull(typeDouble) || isNull(typeMaybe)  ||
	    isNull(typeOrdering)) {
	    ERRMSG(0) "Prelude does not define standard types"
	    EEND;
	}
	stdDefaults  = cons(typeInt,cons(typeDouble,NIL));

	classEq      = findClass(findText("Eq"));
	classOrd     = findClass(findText("Ord"));
	classIx      = findClass(findText("Ix"));
	classEnum    = findClass(findText("Enum"));
	classShow    = findClass(findText("Show"));
	classRead    = findClass(findText("Read"));
	classEval    = findClass(findText("Eval"));
	classBounded = findClass(findText("Bounded"));
	if (isNull(classEq)   || isNull(classOrd) || isNull(classRead) ||
	    isNull(classShow) || isNull(classIx)  || isNull(classEnum) ||
	    isNull(classEval) || isNull(classBounded)) {
	    ERRMSG(0) "Prelude does not define standard classes"
	    EEND;
	}

	classReal       = findClass(findText("Real"));
	classIntegral   = findClass(findText("Integral"));
	classRealFrac   = findClass(findText("RealFrac"));
	classRealFloat  = findClass(findText("RealFloat"));
	classFractional = findClass(findText("Fractional"));
	classFloating   = findClass(findText("Floating"));
	classNum        = findClass(findText("Num"));
	if (isNull(classReal)       || isNull(classIntegral)  ||
	    isNull(classRealFrac)   || isNull(classRealFloat) ||
	    isNull(classFractional) || isNull(classFloating)  ||
	    isNull(classNum)) {
	    ERRMSG(0) "Prelude does not define numeric classes"
	    EEND;
	}
	predNum	        = ap(classNum,var);
	predFractional  = ap(classFractional,var);
	predIntegral    = ap(classIntegral,var);

	classMonad  = findClass(findText("Monad"));
	classMonad0 = findClass(findText("MonadZero"));
	if (isNull(classMonad) || isNull(classMonad0)) {
	    ERRMSG(0) "Prelude does not define monad classes"
	    EEND;
	}
	predMonad  = ap(classMonad,var);
	predMonad0 = ap(classMonad0,var);

#if IO_MONAD
	{   Type typeIO = findTycon(findText("IO"));
	    if (isNull(typeIO)) {
		ERRMSG(0) "Prelude does not define IO monad constructor"
		EEND;
	    }
	    typeProgIO = ap(typeIO,typeUnit);
	}
#endif

	/* The following primitives are referred to in derived instances and
	 * hence require types; the following types are a little more general
	 * than we might like, but they are the closest we can get without a
	 * special datatype class.
	 */
	name(nameConCmp).type
	    = mkPolyType(starToStar,fn(var,fn(var,typeOrdering)));
	name(nameEnRange).type
	    = mkPolyType(starToStar,fn(boundPair,listof));
	name(nameEnIndex).type
	    = mkPolyType(starToStar,fn(boundPair,fn(var,typeInt)));
	name(nameEnInRng).type
	    = mkPolyType(starToStar,fn(boundPair,fn(var,typeBool)));
	name(nameEnToEn).type
	    = mkPolyType(starToStar,fn(var,fn(typeInt,var)));
	name(nameEnFrEn).type
	    = mkPolyType(starToStar,fn(var,typeInt));
	name(nameEnFrom).type
	    = mkPolyType(starToStar,fn(var,listof));
	name(nameEnFrTo).type
	    = name(nameEnFrTh).type
	    = mkPolyType(starToStar,fn(var,fn(var,listof)));

	addEvalInst(0,typeArrow,2,NIL);	/* Add Eval instances for builtins */
	addEvalInst(0,typeList,1,NIL);
	addEvalInst(0,typeUnit,0,NIL);
	for (i=2; i<=NUM_DTUPLES; i++) {/* Add derived instances of tuples */
	    addEvalInst(0,mkTuple(i),i,NIL);
	    addTupInst(classEq,i);
	    addTupInst(classOrd,i);
	    addTupInst(classShow,i);
	    addTupInst(classRead,i);
	    addTupInst(classIx,i);
	}
    }
}

Void linkPreludeCM() {			/* Hook to cfuns and mfuns in	   */
    if (isNull(nameFalse)) {		/* prelude when first loaded	   */
	nameFalse   = findName(findText("False"));
	nameTrue    = findName(findText("True"));
	nameJust    = findName(findText("Just"));
	nameNothing = findName(findText("Nothing"));
	nameLT	    = findName(findText("LT"));
	nameEQ	    = findName(findText("EQ"));
	nameGT	    = findName(findText("GT"));
	if (isNull(nameFalse) || isNull(nameTrue)    ||
	    isNull(nameJust)  || isNull(nameNothing) ||
	    isNull(nameLT)    || isNull(nameEQ)      || isNull(nameGT)) {
	    ERRMSG(0) "Prelude does not define standard constructors"
	    EEND;
	}

	nameFromInt     = findName(findText("fromInt"));
	nameFromInteger = findName(findText("fromInteger"));
	nameFromDouble  = findName(findText("fromDouble"));
	nameEq	        = findName(findText("=="));
	nameCompare     = findName(findText("compare"));
	nameShowsPrec   = findName(findText("showsPrec"));
	nameLe	        = findName(findText("<="));
	nameIndex       = findName(findText("index"));
	nameInRange     = findName(findText("inRange"));
	nameRange       = findName(findText("range"));
	nameMult        = findName(findText("*"));
	namePlus        = findName(findText("+"));
	nameMinBnd	= findName(findText("minBound"));
	nameMaxBnd	= findName(findText("maxBound"));
	nameStrict	= findName(findText("strict"));
	nameSeq		= findName(findText("seq"));
	nameReturn      = findName(findText("return"));
	nameBind        = findName(findText(">>="));
	nameZero        = findName(findText("zero"));
	if (isNull(nameFromInt)   || isNull(nameFromDouble)  ||
	    isNull(nameEq)        || isNull(nameCompare)     ||
	    isNull(nameShowsPrec) || isNull(nameLe)          ||
	    isNull(nameIndex)     || isNull(nameInRange)     ||
	    isNull(nameRange)	  || isNull(nameMult)	     ||
	    isNull(namePlus)      || isNull(nameFromInteger) ||
	    isNull(nameMinBnd)    || isNull(nameMaxBnd)      ||
	    isNull(nameStrict)    || isNull(nameSeq)         ||
	    isNull(nameReturn)	  || isNull(nameBind)	     ||
	    isNull(nameZero)) {
	    ERRMSG(0) "Prelude does not define standard members"
	    EEND;
	}
    }
}

/*-------------------------------------------------------------------------*/
