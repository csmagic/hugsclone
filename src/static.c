/* --------------------------------------------------------------------------
 * static.c:    Copyright (c) Mark P Jones 1991-1996.   All rights reserved.
 *              See NOTICE for details and conditions of use etc...
 *              Hugs version 1.3, August 1996
 *
 * Static Analysis for Hugs
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"

/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/

static Void  local checkTyconDefn	Args((Tycon));
static Void  local depConstrs		Args((Tycon,List,Cell));
static List  local addSels		Args((Int,Name,List,List));
static Type  local depTypeExp		Args((Int,List,Type));
static Type  local depTypeVar		Args((Int,List,Text));
static List  local selectCtxt		Args((List,List));
static Void  local checkSynonyms	Args((List));
static List  local visitSyn		Args((List,Tycon,List));
static Void  local deriveEval		Args((List));
static List  local visitPossEval	Args((Tycon,List,List));
static Void  local checkBanged		Args((Name,Type,List));

static Type  local instantiateSyn	Args((Type,Type));

static List  local typeVarsIn		Args((Cell,List));
static List  local maybeAppendVar	Args((Cell,List));

static List  local offsetTyvarsIn	Args((Type,List));

static Type  local checkSigType		Args((Int,String,Cell,Type));

static Void  local checkClassDefn	Args((Class));
static Void  local depPredExp		Args((Int,List,Cell));
static Void  local checkMems		Args((Cell,List,Cell));
static Void  local addMembers		Args((Class));
static Name  local newMember		Args((Int,Int,Cell,Type));
static Int   local visitClass		Args((Class));

static Void  local checkInstDefn        Args((Inst));
static Void  local checkInstSC		Args((Inst));
static Cell  local scEvidFrom		Args((Cell,List,Int));

static List  local classBindings        Args((String,Class,List));
static Int   local memberNumber         Args((Class,Text));
static List  local numInsert            Args((Int,Cell,List));

static Void  local checkDerive		Args((Tycon,List,List,Cell));
static Void  local addDerInst		Args((Int,Class,List,List,Type,Int));
static Void  local deriveContexts	Args((List));
static Void  local expandDComps		Args((Inst));
static List  local superSimp		Args((List));
static Void  local maybeAddPred		Args((Cell,List));
static Cell  local instPred		Args((Cell,Type));
static Void  local calcInstPreds	Args((Inst));

static Void  local addDerivImp		Args((Inst));
static List  local getDiVars		Args((Int));
static Cell  local mkBind		Args((String,List));
static Cell  local mkVarAlts		Args((Int,Cell));

static List  local deriveEq		Args((Tycon));
static Pair  local mkAltEq		Args((Int,List));
static List  local deriveOrd		Args((Tycon));
static Pair  local mkAltOrd		Args((Int,List));
static List  local makeDPats2		Args((Cell,Int));

static List  local deriveIx		Args((Tycon));
static List  local deriveEnum		Args((Tycon));
static Bool  local isEnumType		Args((Tycon));
static List  local mkIxBinds		Args((Int,Cell,Int));
static Cell  local prodRange		Args((Int,List,Cell,Cell,Cell));
static Cell  local prodIndex		Args((Int,List,Cell,Cell,Cell));
static Cell  local prodInRange		Args((Int,List,Cell,Cell,Cell));

static List  local deriveShow		Args((Tycon));
static Cell  local mkAltShow		Args((Int,Cell,Int));
static Cell  local showsPrecRhs		Args((Cell,Cell));

static List  local deriveRead		Args((Tycon));

static List  local deriveBounded	Args((Tycon));
static List  local mkBndBinds		Args((Int,Cell,Int));

static Void  local checkPrimDefn	Args((Triple));
static Void  local addNewPrim		Args((Int,Text,String,Cell));

static Void  local checkDefaultDefns	Args((Void));

static Cell  local checkPat		Args((Int,Cell));
static Cell  local checkMaybeCnkPat	Args((Int,Cell));
static Cell  local checkApPat		Args((Int,Int,Cell));
static Void  local addPatVar		Args((Int,Cell));
static Name  local conDefined		Args((Int,Text));
static Void  local checkIsCfun		Args((Int,Name));
static Void  local checkCfunArgs	Args((Int,Cell,Int));

static Cell  local bindPat		Args((Int,Cell));
static Void  local bindPats		Args((Int,List));

static List  local extractSigdecls	Args((List));
static List  local extractBindings	Args((List));
static List  local eqnsToBindings	Args((List));
static Void  local notDefined		Args((Int,List,Cell));
static Cell  local findBinding		Args((Text,List));
static Void  local addSigDecl		Args((List,Cell));
static Void  local setType		Args((Int,Cell,Cell,List));

static List  local dependencyAnal	Args((List));
static List  local topDependAnal	Args((List));
static Void  local addDepField		Args((Cell));
static Void  local remDepField		Args((List));
static Void  local remDepField1		Args((Cell));
static Void  local clearScope		Args((Void));
static Void  local withinScope		Args((List));
static Void  local leaveScope		Args((Void));

static Void  local depBinding		Args((Cell));
static Void  local depDefaults          Args((Class));
static Void  local depInsts             Args((Inst));
static Void  local depClassBindings     Args((List));
static Void  local depAlt		Args((Cell));
static Void  local depRhs		Args((Cell));
static Void  local depGuard		Args((Cell));
static Cell  local depExpr		Args((Int,Cell));
static Void  local depPair		Args((Int,Cell));
static Void  local depTriple		Args((Int,Cell));
static Void  local depComp		Args((Int,Cell,List));
static Void  local depCaseAlt		Args((Int,Cell));
static Cell  local depVar		Args((Int,Cell));
static Void  local depConFlds		Args((Int,Cell,Bool));
static Void  local depUpdFlds		Args((Int,Cell));
static List  local depFields		Args((Int,Cell,List,Bool));

static Int   local sccMin		Args((Int,Int));
static List  local tcscc		Args((List,List));
static List  local bscc			Args((List));

static Void  local addRSsigdecls	Args((Pair));
static Void  local opDefined		Args((List,Cell));
static Void  local allNoPrevDef		Args((Cell));
static Void  local noPrevDef		Args((Int,Cell));
static Void  local checkTypeIn		Args((Pair));

/* --------------------------------------------------------------------------
 * Static analysis of type declarations:
 *
 * Type declarations come in two forms:
 * - data declarations - define new constructed data types
 * - type declarations - define new type synonyms
 *
 * A certain amount of work is carried out as the declarations are
 * read during parsing.  In particular, for each type constructor
 * definition encountered:
 * - check that there is no previous definition of constructor
 * - ensure type constructor not previously used as a class name
 * - make a new entry in the type constructor table
 * - record line number of declaration
 * - Build separate lists of newly defined constructors for later use.
 * ------------------------------------------------------------------------*/

Void tyconDefn(line,lhs,rhs,what)	/* process new type definition	   */
Int  line;				/* definition line number	   */
Cell lhs;				/* left hand side of definition	   */
Cell rhs;				/* right hand side of definition   */
Cell what; {				/* SYNONYM/DATATYPE/etc...	   */
    Text t = textOf(getHead(lhs));

    if (nonNull(findTycon(t))) {
	ERRMSG(line) "Repeated definition of type constructor \"%s\"",
		     textToStr(t)
	EEND;
    }
    else if (nonNull(findClass(t))) {
	ERRMSG(line) "\"%s\" used as both class and type constructor",
		     textToStr(t)
	EEND;
    }
    else {
	Tycon nw	= newTycon(t);
	tyconDefns      = cons(nw,tyconDefns);
	tycon(nw).line  = line;
	tycon(nw).arity = argCount;
	tycon(nw).what  = what;
	if (what==RESTRICTSYN) {
	    typeInDefns = cons(pair(nw,snd(rhs)),typeInDefns);
	    rhs         = fst(rhs);
	}
	tycon(nw).defn  = pair(lhs,rhs);
    }
}

Void setTypeIns(bs)			/* set local synonyms for given	   */
List bs; {				/* binding group		   */
    List cvs = typeInDefns;
    for (; nonNull(cvs); cvs=tl(cvs)) {
	Tycon c  = fst(hd(cvs));
	List  vs = snd(hd(cvs));
	for (tycon(c).what = RESTRICTSYN; nonNull(vs); vs=tl(vs)) {
	    if (nonNull(findBinding(textOf(hd(vs)),bs))) {
		tycon(c).what = SYNONYM;
		break;
	    }
	}
    }
}

Void clearTypeIns() {			/* clear list of local synonyms	   */
    for (; nonNull(typeInDefns); typeInDefns=tl(typeInDefns))
	tycon(fst(hd(typeInDefns))).what = RESTRICTSYN;
}

/* --------------------------------------------------------------------------
 * Further analysis of Type declarations:
 *
 * In order to allow the definition of mutually recursive families of
 * data types, the static analysis of the right hand sides of type
 * declarations cannot be performed until all of the type declarations
 * have been read.
 *
 * Once parsing is complete, we carry out the following:
 *
 * - check format of lhs, extracting list of bound vars and ensuring that
 *   there are no repeated variables and no Skolem variables.
 * - run dependency analysis on rhs to check that only bound type vars
 *   appear in type and that all constructors are defined.
 *   Replace type variables by offsets, constructors by Tycons.
 * - use list of dependents to sort into strongly connected components.
 * - ensure that there is not more than one synonym in each group.
 * - kind-check each group of type definitions.
 *
 * - check that there are no previous definitions for constructor
 *   functions in data type definitions.
 * - install synonym expansions and constructor definitions.
 * ------------------------------------------------------------------------*/

static List tcDeps = NIL;		/* list of dependent tycons/classes*/

static Void local checkTyconDefn(d)	/* validate type constructor defn  */
Tycon d; {
    Cell lhs    = fst(tycon(d).defn);
    Cell rhs    = snd(tycon(d).defn);
    Int  line   = tycon(d).line;
    List tyvars = getArgs(lhs);
    List temp;
					/* check for repeated tyvars on lhs*/
    for (temp=tyvars; nonNull(temp); temp=tl(temp))
	if (nonNull(varIsMember(textOf(hd(temp)),tl(temp)))) {
	    ERRMSG(line) "Repeated type variable \"%s\" on left hand side",
			 textToStr(textOf(hd(temp)))
	    EEND;
	}

    tcDeps = NIL;			/* find dependents		   */
    switch (whatIs(tycon(d).what)) {
	case RESTRICTSYN :
	case SYNONYM	 : rhs = depTypeExp(line,tyvars,rhs);
			   if (cellIsMember(d,tcDeps)) {
			       ERRMSG(line) "Recursive type synonym \"%s\"",
					    textToStr(tycon(d).text)
			       EEND;
			   }
			   break;

	case DATATYPE	 :
	case NEWTYPE	 : depConstrs(d,tyvars,rhs);
			   rhs = fst(rhs);
			   break;

	default		 : internal("checkTyconDefn");
			   break;
    }

    tycon(d).defn = rhs;
    tycon(d).kind = tcDeps;
    tcDeps	  = NIL;
}

static Void local depConstrs(t,tyvars,cd)
Tycon t;				/* Define constructor functions and*/
List  tyvars;				/* do dependency analysis for data */
Cell  cd; {				/* definitions (w or w/o deriving) */
    Int  line      = tycon(t).line;
    List ctxt      = NIL;
    Int  conNo     = 1;
    Type lhs	   = t;
    List cs	   = fst(cd);
    List derivs    = snd(cd);
    List compTypes = NIL;
    List sels	   = NIL;
    Int  i;

    for (i=0; i<tycon(t).arity; ++i)	/* build representation for tycon  */
	lhs = ap(lhs,mkOffset(i));	/* applied to full comp. of args   */

    if (whatIs(cs)==QUAL) {		/* allow for possible context	   */
	ctxt = fst(snd(cs));
	cs   = snd(snd(cs));
	map2Proc(depPredExp,line,tyvars,ctxt);
    }

    if (nonNull(cs) && isNull(tl(cs)))	/* Single constructor datatype?	   */
	conNo = 0;

    for (; nonNull(cs); cs=tl(cs)) {	/* For each constructor function:  */
	Cell con   = hd(cs);
	List sig   = dupList(tyvars);
	List ctxt1 = ctxt;		/* constructor function context	   */
	List scs   = NIL;		/* strict components		   */
	List fs    = NONE;		/* selector names		   */
	Type type  = lhs;		/* constructor function type	   */
	Int  arity = 0;			/* arity of constructor function   */
	Name n;				/* name for constructor function   */

	if (whatIs(con)==LABC) {	/* Skeletize constr components	   */
	    Cell fls = snd(snd(con));	/* get field specifications	   */
	    con      = fst(snd(con));
	    fs	     = NIL;
	    for (; nonNull(fls); fls=tl(fls)) { /* for each field spec:    */
		List vs     = fst(hd(fls));
		Type t      = snd(hd(fls));	/* - scrutinize type	   */
		Bool banged = whatIs(t)==BANG;
		t           = depTypeExp(line,sig,(banged ? arg(t) : t));
		while (nonNull(vs)) {		/* - add named components  */
		    Cell us = tl(vs);
		    tl(vs)  = fs;
		    fs      = vs;
		    vs      = us;
		    con     = ap(con,t);
		    arity++;
		    if (banged)
			scs = cons(mkInt(arity),scs);
		}
	    }
	    fs  = rev(fs);
	    scs = rev(scs);		/* put strict comps in ascend ord  */
	}
	else {				/* Non-labelled constructor	   */
	    Cell c = con;
	    Int  compNo;
	    for (; isAp(c); c=fun(c))
		arity++;
	    for (compNo=arity, c=con; isAp(c); c=fun(c)) {
		Type t = arg(c);
		if (whatIs(t)==BANG) {
		    scs = cons(mkInt(compNo),scs);
		    t   = arg(t);
		}
		compNo--;
		arg(c) = depTypeExp(line,sig,t);
	    }
	}

	if (nonNull(ctxt1))		/* Extract relevant part of context*/
	    ctxt1 = selectCtxt(ctxt1,offsetTyvarsIn(con,NIL));

	while (isAp(con)) {		/* Calculate type of constructor   */
	    Type t   = fun(con);
	    fun(con) = typeArrow;
	    if (nonNull(derivs))	/* and build list of components	   */
		compTypes = cons(arg(con),compTypes);
	    type     = ap(con,type);
	    con      = t;
	}

	if (nonNull(ctxt1))		/* Add context part to type	   */
	    type = ap(QUAL,pair(ctxt1,type));

	if (nonNull(sig)) {		/* Add quantifiers to type	   */
	    List ts1 = tyvars;
	    List ts2 = sig;
	    for (; nonNull(ts1); ts1=tl(ts1), ts2=tl(ts2))
		hd(ts2) = NIL;
	    type = mkPolyType(sig,type);
	}

	n = findName(textOf(con));	/* Allocate constructor fun name   */
	if (isNull(n))
	    n = newName(textOf(con));
	else if (name(n).defn!=PREDEFINED) {
	    ERRMSG(line) "Repeated definition for constructor function \"%s\"",
			 textToStr(name(n).text)
	    EEND;
	}
	name(n).arity  = arity;		/* Save constructor fun details	   */
	name(n).line   = line;
	name(n).number = cfunNo(conNo++);
	name(n).type   = type;
	if (tycon(t).what==NEWTYPE)
	    name(n).defn = nameId;
	else
	    implementCfun(n,scs);

	hd(cs) = n;
	if (fs!=NONE)
	    sels = addSels(line,n,fs,sels);
    }

    if (nonNull(sels)) {
	sels     = rev(sels);
	fst(cd)  = appendOnto(fst(cd),sels);
	selDefns = cons(sels,selDefns);
    }

    if (nonNull(derivs)) {		/* Generate derived instances	   */
	map3Proc(checkDerive,t,ctxt,compTypes,derivs);
    }
}

static List cfunSfuns;			/* List of (Cfun,[SelectorVar])	   */
					/*  - used for deriving Show	   */

static List local addSels(line,c,fs,ss)	/* Add fields to selector list	   */
Int  line;				/* line number of constructor	   */
Name c;					/* corresponding constr function   */
List fs;				/* list of fields (varids)	   */
List ss; {				/* list of existing selectors	   */
    Int sn    = 1;
    cfunSfuns = cons(pair(c,fs),cfunSfuns);
    for (; nonNull(fs); fs=tl(fs), ++sn) {
	List ns = ss;
	Text t  = textOf(hd(fs));

	if (nonNull(varIsMember(t,tl(fs)))) {
	    ERRMSG(line) "Repeated field name \"%s\" for constructor \"%s\"",
			 textToStr(t), textToStr(name(c).text)
	    EEND;
	}

	while (nonNull(ns) && t!=name(hd(ns)).text)
	    ns = tl(ns);

	if (nonNull(ns))
	    name(hd(ns)).defn = cons(pair(c,mkInt(sn)),name(hd(ns)).defn);
	else {
	    Name n = findName(t);
	    if (nonNull(n)) {
	    	ERRMSG(line) "Repeated definition for selector \"%s\"",
			     textToStr(t)
		EEND;
	    }
	    n              = newName(t);
	    name(n).line   = line;
	    name(n).number = SELNAME;
	    name(n).defn   = singleton(pair(c,mkInt(sn)));
	    ss             = cons(n,ss);
	}
    }
    return ss;
}

static Type local depTypeExp(line,tyvars,type)
Int  line;
List tyvars;
Type type; {
    switch (whatIs(type)) {
	case AP		: fst(type) = depTypeExp(line,tyvars,fst(type));
			  snd(type) = depTypeExp(line,tyvars,snd(type));
			  break;

	case VARIDCELL	: return depTypeVar(line,tyvars,textOf(type));

	case CONIDCELL	: {   Tycon tc = findTycon(textOf(type));
			      if (isNull(tc)) {
				  ERRMSG(line)
				      "Undefined type constructor \"%s\"",
				      textToStr(textOf(type))
				  EEND;
			      }
			      if (cellIsMember(tc,tyconDefns) &&
				  !cellIsMember(tc,tcDeps))
				  tcDeps = cons(tc,tcDeps);
			      return tc;
			  }

	case TYCON	:
	case TUPLE	: break;

	default		: internal("depTypeExp");
    }
    return type;
}

static Type local depTypeVar(line,tyvars,tv)
Int  line;
List tyvars;
Text tv; {
    Int offset = 0;

    for (; nonNull(tyvars) && tv!=textOf(hd(tyvars)); offset++)
	tyvars = tl(tyvars);
    if (isNull(tyvars)) {
	ERRMSG(line) "Undefined type variable \"%s\"", textToStr(tv)
	EEND;
    }
    return mkOffset(offset);
}
 
static List local selectCtxt(ctxt,vs)	/* calculate subset of context	   */
List ctxt;
List vs; {
    if (isNull(vs))
	return NIL;
    else {
	List ps = NIL;
	for (; nonNull(ctxt); ctxt=tl(ctxt)) {
	    List us = offsetTyvarsIn(hd(ctxt),NIL);
	    for (; nonNull(us) && cellIsMember(hd(us),vs); us=tl(us))
		;
	    if (isNull(us))
		ps = cons(hd(ctxt),ps);
	}
	return rev(ps);
    }
}

static Void local checkSynonyms(ts)	/* Check for mutually recursive	   */
List ts; {				/* synonyms			   */
    List syns = NIL;
    for (; nonNull(ts); ts=tl(ts)) {	/* build list of all synonyms	   */
	Tycon t = hd(ts);
	switch (whatIs(tycon(t).what)) {
	    case SYNONYM     :
	    case RESTRICTSYN : syns = cons(t,syns);
			       break;
	}
    }
    while (nonNull(syns))		/* then visit each synonym	   */
	syns = visitSyn(NIL,hd(syns),syns);
}

static List local visitSyn(path,t,syns)	/* visit synonym definition to look*/
List  path;				/* for cycles			   */
Tycon t;
List  syns; {
    if (cellIsMember(t,path)) {		/* every elt in path depends on t  */
	ERRMSG(tycon(t).line)
	    "Type synonyms \"%s\" and \"%s\" are mutually recursive",
	    textToStr(tycon(t).text), textToStr(tycon(hd(path)).text)
	EEND;
    }
    else {
	List ds    = tycon(t).kind;
        List path1 = NIL;
	for (; nonNull(ds); ds=tl(ds))
	    if (cellIsMember(hd(ds),syns)) {
		if (isNull(path1))
		    path1 = cons(t,path);
		syns = visitSyn(path1,hd(ds),syns);
	    }
    }
    tycon(t).defn = fullExpand(tycon(t).defn);
    return removeCell(t,syns);
}

static Void local deriveEval(tcs)	/* Derive instances of Eval	   */
List tcs; {
    List ts1 = tcs;
    List ts  = NIL;
    for (; nonNull(ts1); ts1=tl(ts1)) {	/* Build list of rsyns and newtypes*/
	Tycon t = hd(ts1);		/* and derive instances for data   */
	switch (whatIs(tycon(t).what)) {
	    case DATATYPE    : addEvalInst(tycon(t).line,t,tycon(t).arity,NIL);
			       break;
	    case NEWTYPE     :
	    case RESTRICTSYN : ts = cons(t,ts);
			       break;
	}
    }
    while (nonNull(ts))			/* ... then visit each in turn	   */
	ts = visitPossEval(hd(ts),tl(ts),NIL);

    for (; nonNull(tcs); tcs=tl(tcs)) {	/* Check any banged components	   */
	Tycon t = hd(tcs);
	if (whatIs(tycon(t).what)==DATATYPE) {
	    List cs = tycon(t).defn;
	    for (; hasCfun(cs); cs=tl(cs)) {
		Name c = hd(cs);
		if (isPair(name(c).defn)) {
		    Type t    = name(c).type;
		    List scs  = fst(name(c).defn);
		    List ctxt = NIL;
		    Int  n    = 1;
		    if (isPolyType(t))
			t = monoTypeOf(t);
		    if (whatIs(t)==QUAL) {
			ctxt = fst(snd(t));
			t    = snd(snd(t));
		    }
		    for (; nonNull(scs); scs=tl(scs)) {
			Int i = intOf(hd(scs));
			for (; n<i; n++)
				t = arg(t);
			checkBanged(c,arg(fun(t)),ctxt);
		    }
		}
	    }
	}
    }
}

static List local visitPossEval(t,ts,ps)/* Test to see if we can add an	   */
Tycon t;				/* Eval instance for t		   */
List  ts;				/* [RESTRICTSYN | NEWTYPE]	   */
List  ps; {				/* [Tycons already being visited]  */
    Type ty = tycon(t).defn;		/* Find the `deciding type' ...	   */
    if (whatIs(tycon(t).what)==NEWTYPE) {
	ty = name(hd(tycon(t).defn)).type;
	if (isPolyType(ty))
	    ty = monoTypeOf(ty);
	if (whatIs(ty)==QUAL)
	    ty = snd(snd(ty));
	ty = arg(fun(ty));
    }

    for (;;) {				/* Scrutinize it ...		   */
	Type h = getHead(ty);		/* Find the constructor in the	   */
	if (isSynonym(h))		/* head position of the type	   */
	    h = getHead(ty=fullExpand(ty));
	if (isOffset(h)) {		/* Variable in head position	   */
	    if (argCount==0)		/* ... without any arguments	   */
		addEvalInst(tycon(t).line,
			    t,
			    tycon(t).arity,
			    singleton(ap(classEval,h)));
	    return ts;			/* ... with args, so no instance   */
	}
	else {				/* Tycon or tuple in head position */
	    Int  a = argCount;
	    Inst in;

	    if (h==t || cellIsMember(h,ps)) {	/* ... already visited?	   */
		addEvalInst(tycon(t).line,t,tycon(t).arity,NIL);
		return ts;
	    }

	    if (cellIsMember(h,ts))		/* ... still to be visited */
		ts = visitPossEval(h,removeCell(h,ts),cons(t,ps));

	    if (nonNull(in=findInst(classEval,h))) {
		if (nonNull(inst(in).specifics)) {
		    Int n = offsetOf(arg(hd(inst(in).specifics)));
		    while (++n < a)
			ty = fun(ty);
		    ty = arg(ty);	/* If there was a context, then we */
		    continue;		/* need to go round the loop again */
		}
		addEvalInst(tycon(t).line,t,tycon(t).arity,NIL);
	    }
	    return ts;
	}
    }
}

static Void local checkBanged(c,ty,ps)	/* Check that banged component of c */
Name c;					/* with type ty is an instance of   */
Type ty;				/* Eval under the predicates in ps. */
List ps; {
    Type ty1 = ty;			/* Save orig type, in case of err. */
    for (;;) {
	Type h = getHead(ty);		/* Find the constructor in the	   */
	if (isSynonym(h))		/* head position of the type	   */
	    h = getHead(ty=fullExpand(ty));
	if (isOffset(h)) {		/* Variable in head position	   */
	    if (argCount==0 && nonNull(scEvidFrom(pair(classEval,h),ps,0)))
		return;
	    break;
	}
	else {				/* Tycon or tuple in head position */
	    Int  a  = argCount;
	    Inst in = findInst(classEval,h);
	    if (nonNull(in)) {
		if (nonNull(inst(in).specifics)) {
		    Int n = offsetOf(arg(hd(inst(in).specifics)));
		    while (++n < a)
			ty = fun(ty);
		    ty = arg(ty);	/* If there was a context, then we */
		    continue;		/* need to go round the loop again */
		}
		return;			/* No context, so nothing to prove */
	    }
	    break;
	}
    }
    ERRMSG(name(c).line) "Illegal datatype strictness annotation:" ETHEN
    ERRTEXT "\n*** Constructor : "  ETHEN ERREXPR(c);
    ERRTEXT "\n*** Context     : "  ETHEN ERRCONTEXT(ps);
    ERRTEXT "\n*** Required    : "  ETHEN ERRPRED(ap(classEval,ty1));
    ERRTEXT "\n"
    EEND;
}

/* --------------------------------------------------------------------------
 * Expanding out all type synonyms in a type expression:
 * ------------------------------------------------------------------------*/

Type fullExpand(t)			/* find full expansion of type exp */
Type t; {				/* assuming that all relevant      */
    Cell h = t;				/* synonym defns of lower rank have*/
    Int  n = 0;				/* already been fully expanded	   */
    List args;
    for (args=NIL; isAp(h); h=fun(h), n++)
	args = cons(fullExpand(arg(h)),args);
    t = applyToArgs(h,args);
    if (isSynonym(h) && n>=tycon(h).arity)
	if (n==tycon(h).arity)
	    t = instantiateSyn(tycon(h).defn,t);
	else {
	    Type p = t;
	    while (--n > tycon(h).arity)
		p = fun(p);
	    fun(p) = instantiateSyn(tycon(h).defn,fun(p));
	}
    return t;
}

static Type local instantiateSyn(t,env)	/* instantiate type according using*/
Type t;					/* env to determine appropriate    */
Type env; {				/* values for OFFSET type vars	   */
    switch (whatIs(t)) {
	case AP      : return ap(instantiateSyn(fun(t),env),
				 instantiateSyn(arg(t),env));

	case OFFSET  : return nthArg(offsetOf(t),env);

	default	     : return t;
    }
}

/* --------------------------------------------------------------------------
 * Calculate set of variables appearing in a given type expression (possibly
 * qualified) as a list of distinct values.  The order in which variables
 * appear in the list is the same as the order in which those variables
 * occur in the type expression when read from left to right.
 * ------------------------------------------------------------------------*/

static List local typeVarsIn(type,vs)  /* calculate list of type variables */
Cell type;			       /* used in type expression, reading */
List vs; {			       /* from left to right		   */
    switch (whatIs(type)) {
	case AP        : return typeVarsIn(snd(type),
					   typeVarsIn(fst(type),
						      vs));
	case VARIDCELL :
	case VAROPCELL : return maybeAppendVar(type,vs);

	case QUAL      : {   List qs = fst(snd(type));
			     vs = typeVarsIn(snd(snd(type)),vs);
			     for (; nonNull(qs); qs=tl(qs))
				 vs = typeVarsIn(hd(qs),vs);
			     return vs;
			 }

	case BANG      : return typeVarsIn(snd(type),vs);

	case LABC      : {   List fs = snd(snd(type));
			     for (; nonNull(fs); fs=tl(fs))
				vs = typeVarsIn(snd(hd(fs)),vs);
			     return vs;
			 }
    }
    return vs;
}

static List local maybeAppendVar(v,vs) /* append variable to list if not   */
Cell v; 			       /* already included		   */
List vs; {
    Text t = textOf(v);
    List p = NIL;
    List c = vs;

    while (nonNull(c)) {
	if (textOf(hd(c))==t)
	    return vs;
	p = c;
	c = tl(c);
    }

    if (nonNull(p))
	tl(p) = cons(v,NIL);
    else
	vs    = cons(v,NIL);

    return vs;
}

/* --------------------------------------------------------------------------
 * Check for ambiguous types:
 * A type  Preds => type  is ambiguous if not (TV(P) `subset` TV(type))
 * ------------------------------------------------------------------------*/

static List local offsetTyvarsIn(t,vs)	/* add list of offset tyvars in t  */
Type t;					/* to list vs			   */
List vs; {
    switch (whatIs(t)) {
	case AP	    : return offsetTyvarsIn(fun(t),offsetTyvarsIn(snd(t),vs));

	case OFFSET : if (cellIsMember(t,vs))
			  return vs;
		      else
			  return cons(t,vs);

	case QUAL   : return offsetTyvarsIn(snd(t),vs);

	default	    : return vs;
    }
}

Bool isAmbiguous(type)			/* Determine whether type is	   */
Type type; {				/* ambiguous 			   */
    if (isPolyType(type))
	type = monoTypeOf(type);
    if (whatIs(type)==QUAL) {		/* only qualified types can be	   */
	List tvps = offsetTyvarsIn(fst(snd(type)),NIL);	/* ambiguous	   */
	List tvts = offsetTyvarsIn(snd(snd(type)),NIL);
	while (nonNull(tvps) && cellIsMember(hd(tvps),tvts))
	    tvps = tl(tvps);
	return nonNull(tvps);
    }
    return FALSE;
}

Void ambigError(line,where,e,type)	/* produce error message for	   */
Int    line;				/* ambiguity			   */
String where;
Cell   e;
Type   type; {
    ERRMSG(line) "Ambiguous type signature in %s", where ETHEN
    ERRTEXT "\n*** ambiguous type : " ETHEN ERRTYPE(type);
    ERRTEXT "\n*** assigned to    : " ETHEN ERREXPR(e);
    ERRTEXT "\n"
    EEND;
}

/* --------------------------------------------------------------------------
 * Type expressions appearing in type signature declarations and expressions
 * also require static checking, but unlike type expressions in type decls,
 * they may introduce arbitrary new type variables.  The static analysis
 * required here is:
 *   - ensure that each type constructor is defined and used with the
 *     correct number of arguments.
 *   - replace type variables by offsets, constructor names by Tycons.
 *   - ensure that type is well-kinded.
 * ------------------------------------------------------------------------*/

static Type local checkSigType(line,where,e,type)
Int    line;			       /* check validity of type expression*/
String where;			       /* in explicit type signature	   */
Cell   e;
Type   type; {
    List tyvars = typeVarsIn(type,NIL);
    Int  n      = length(tyvars);

    if (whatIs(type)==QUAL) {
	map2Proc(depPredExp,line,tyvars,fst(snd(type)));
	snd(snd(type)) = depTypeExp(line,tyvars,snd(snd(type)));

	if (isAmbiguous(type))
	    ambigError(line,where,e,type);
    }
    else
	type = depTypeExp(line,tyvars,type);

    if (n>0)
	if (n>=NUM_OFFSETS) {
	    ERRMSG(line) "Too many type variables in %s\n", where
	    EEND;
	}
	else {
	    List ts = tyvars;
	    for (; nonNull(ts); ts=tl(ts))
		hd(ts) = NIL;
	    type    = mkPolyType(tyvars,type);
	}

    kindSigType(line,type);		/* check that type is well-kinded  */
    return type;
}

/* --------------------------------------------------------------------------
 * Static analysis of class declarations:
 *
 * Performed in a similar manner to that used for type declarations.
 *
 * The first part of the static analysis is performed as the declarations
 * are read during parsing.  The parser ensures that:
 * - the class header and all superclass predicates are of the form
 *   ``Class var''
 *
 * The classDefn() function:
 * - ensures that there is no previous definition for class
 * - checks that class name has not previously been used as a type constr.
 * - make new entry in class table
 * - record line number of declaration
 * - build list of classes defined in current script for use in later
 *   stages of static analysis.
 * ------------------------------------------------------------------------*/

Void classDefn(line,head,ms)	       /* process new class definition	   */
Int  line;			       /* definition line number	   */
Cell head;			       /* class header :: ([Supers],Class) */
List ms; {			       /* class definition body		   */
    Text ct = textOf(fun(snd(head)));

    if (nonNull(findClass(ct))) {
	ERRMSG(line) "Repeated definition of class \"%s\"",
		     textToStr(ct)
	EEND;
    }
    else if (nonNull(findTycon(ct))) {
	ERRMSG(line) "\"%s\" used as both class and type constructor",
		     textToStr(ct)
	EEND;
    }
    else {
	Class nw	   = newClass(ct);
	cclass(nw).line    = line;
	cclass(nw).supers  = head;
	cclass(nw).members = ms;
	cclass(nw).level   = 0;
	classDefns	   = cons(nw,classDefns);
    }
}

/* --------------------------------------------------------------------------
 * Further analysis of class declarations:
 *
 * Full static analysis of class definitions must be postponed until the
 * complete script has been read and all static analysis on type definitions
 * has been completed.
 *
 * Once this has been achieved, we carry out the following checks on each
 * class definition:
 * - check superclass declarations, replace by list of classes
 * - split body of class into members and declarations
 * - make new name entry for each member function
 * - record member function number (eventually an offset into dictionary!)
 * - no member function has a previous definition ...
 * - no member function is mentioned more than once in the list of members
 * - each member function type is valid, replace vars by offsets
 * - qualify each member function type by class header
 * - only bindings for members appear in defaults
 * - only function bindings appear in defaults
 * - check that extended class hierarchy does not contain any cycles
 * ------------------------------------------------------------------------*/

static Void local checkClassDefn(c)    /* validate class definition	   */
Class c; {
    Cell head          = snd(cclass(c).supers);
    List tyvars        = singleton(arg(head));

    cclass(c).supers   = fst(cclass(c).supers);			/* supercl.*/
    tcDeps	       = NIL;
    map2Proc(depPredExp,cclass(c).line,tyvars,cclass(c).supers);
    cclass(c).numSupers= length(cclass(c).supers);
    mapOver(fst,cclass(c).supers);

    cclass(c).defaults = extractBindings(cclass(c).members);	/* defaults*/
    cclass(c).members  = extractSigdecls(cclass(c).members);
    fun(head)	       = c;
    arg(head)	       = mkOffset(0);
    map2Proc(checkMems,head,tyvars,cclass(c).members);
    cclass(c).sig      = tcDeps;
    tcDeps             = NIL;
}

static Void local depPredExp(line,tyvars,pred)
Int  line;
List tyvars;
Cell pred; {
    Class c = findClass(textOf(fun(pred)));
    if (isNull(c)) {
	ERRMSG(line) "Undefined class \"%s\"", textToStr(textOf(fun(pred)))
	EEND;
    }
    fun(pred) = c;
    arg(pred) = depTypeExp(line,tyvars,arg(pred));
    if (cellIsMember(c,classDefns) && !cellIsMember(c,tcDeps))
	tcDeps = cons(c,tcDeps);
}

static Void local checkMems(h,tyvars,m)	/* check member function details   */
Cell h;
List tyvars;
Cell m; {
    Int  line = intOf(fst3(m));
    List vs   = snd3(m);
    Type t    = thd3(m);

    tyvars    = typeVarsIn(t,tyvars);
    if (whatIs(t)==QUAL) {		/* overloaded member signatures?  */
	List qs = fst(snd(t));
	for (; nonNull(qs); qs=tl(qs)) {
	    depPredExp(line,tyvars,hd(qs));
	    if (arg(hd(qs))==mkOffset(0)) {
		ERRMSG(line) "Illegal constraints on class variable \"%s\"",
			     textToStr(textOf(hd(tyvars)))
		ETHEN ERRTEXT " in type of member function \"%s\"",
			    textToStr(textOf(hd(vs)))
		EEND;
	    }
	}
    }
    else
	t = ap(QUAL,pair(NIL,t));

    fst(snd(t)) = cons(h,fst(snd(t)));		/* Add main predicate	   */
    snd(snd(t)) = depTypeExp(line,tyvars,snd(snd(t)));

    if (isNull(tl(tyvars)))			
	t = mkPolyType(singleton(NIL),t);
    else {
	List sig   = NIL;
	List tvs   = tyvars;
	for (; nonNull(tvs); tvs=tl(tvs))
	    sig = ap(NIL,sig);
	t          = mkPolyType(rev(sig),t);
	tl(tyvars) = NIL;			/* delete extra type vars  */
    }

    if (isAmbiguous(t))
	ambigError(line,"class declaration",hd(vs),t);

    thd3(m)    = t;				/* save type		   */
}

static Void local addMembers(c)		/* Add definitions of member funs  */
Class c; {
    Int  mno   = 1;			/* member function number	   */
    List mfuns = NIL;			/* list of member functions	   */
    List ms    = cclass(c).members;

    for (; nonNull(ms); ms=tl(ms)) {	/* cycle through each sigdecl	   */
	Int  line = intOf(fst3(hd(ms)));
	List vs   = rev(snd3(hd(ms)));
	Type t    = thd3(hd(ms));
	for (; nonNull(vs); vs=tl(vs))
	    mfuns = cons(newMember(line,mno++,hd(vs),t),mfuns);
    }
    cclass(c).members    = rev(mfuns);	/* save list of members		   */
    cclass(c).numMembers = length(cclass(c).members);
    cclass(c).defaults   = classBindings("class",c,cclass(c).defaults);
}

static Name local newMember(l,no,v,t)	/* Make definition for member fn   */
Int  l;
Int  no;
Cell v;
Type t; {
    Name m = findName(textOf(v));

    if (isNull(m))
	m = newName(textOf(v));
    else if (name(m).defn!=PREDEFINED) {
	ERRMSG(l) "Repeated definition for member function \"%s\"",
		  textToStr(name(m).text)
	EEND;
    }

    name(m).line   = l;
    name(m).arity  = 1;
    name(m).number = mfunNo(no);
    name(m).type   = t;
    name(m).defn   = NIL;
    return m;
}

static Int local visitClass(c)		/* visit class defn to check that  */
Class c; {				/* class hierarchy is acyclic	   */
    if (cclass(c).level < 0) {		/* already visiting this class?	   */
	ERRMSG(cclass(c).line) "Class hierarchy for \"%s\" is not acyclic",
			       textToStr(cclass(c).text)
	EEND;
    }
    else if (cclass(c).level == 0) {	/* visiting class for first time   */
	List scs = cclass(c).supers;
	Int  lev = 0;
	cclass(c).level = (-1);
	for (; nonNull(scs); scs=tl(scs)) {
	    Int l = visitClass(hd(scs));
	    if (l>lev) lev=l;
	}
	cclass(c).level = 1+lev;	/* level = 1 + max level of supers */
    }
    return cclass(c).level;
}

/* --------------------------------------------------------------------------
 * Static analysis of instance declarations:
 *
 * The first part of the static analysis is performed as the declarations
 * are read during parsing:
 * - make new entry in instance table
 * - record line number of declaration
 * - build list of instances defined in current script for use in later
 *   stages of static analysis.
 * ------------------------------------------------------------------------*/

Void instDefn(line,head,ms)	       /* process new instance definition  */
Int  line;			       /* definition line number	   */
Cell head;			       /* inst header :: (context,Class)   */
List ms; {			       /* instance members		   */
    Inst nw             = newInst();
    inst(nw).line       = line;
    inst(nw).specifics  = head;
    inst(nw).implements = ms;
    instDefns           = cons(nw,instDefns);
}

/* --------------------------------------------------------------------------
 * Further static analysis of instance declarations:
 *
 * Makes the following checks:
 * - Class part of header has form C (T a1 ... an) where C is a known
 *   class, and T is a known datatype constructor (or restricted synonym),
 *   and there is no previous C-T instance, and (T a1 ... an) has a kind
 *   appropriate for the class C.
 * - Each element of context is a valid class expression, with type vars
 *   drawn from a1, ..., an.
 * - All bindings are function bindings
 * - All bindings define member functions for class C
 * - Arrange bindings into appropriate order for member list
 * - No top level type signature declarations
 * ------------------------------------------------------------------------*/

static Void local checkInstDefn(in)    /* validate instance declaration    */
Inst in; {
    Int  line   = inst(in).line;
    Cell head   = snd(inst(in).specifics);
    List tyvars = getArgs(arg(head));
    Cell tmp;

    for (tmp=tyvars; nonNull(tmp); tmp=tl(tmp))	/* check for repeated var  */
	if (nonNull(varIsMember(textOf(hd(tmp)),tl(tmp)))) {
	    ERRMSG(line) "Repeated type variable \"%s\" in instance predicate",
			 textToStr(textOf(hd(tmp)))
	    EEND;
	}
    depPredExp(line,tyvars,head);
    if (fun(head)==classEval) {
	ERRMSG(line) "Instances of class \"%s\" are generated automatically",
		     textToStr(cclass(fun(head)).text)
	EEND;
    }
    inst(in).specifics = fst(inst(in).specifics);
    map2Proc(depPredExp,line,tyvars,inst(in).specifics);
    inst(in).numSpecifics = length(inst(in).specifics);

    tmp = getHead(arg(head));
    if (!isTycon(tmp) && !isTuple(tmp)) {
	ERRMSG(line) "Simple type required in instance declaration"
	EEND;
    }
    if (isSynonym(tmp)) {
	ERRMSG(line) "Type synonym \"%s\" not permitted in instance of \"%s\"",
		     textToStr(tycon(tmp).text),
		     textToStr(cclass(fun(head)).text)
	EEND;
    }

    inst(in).c     = fun(head);
    inst(in).t     = tmp;
    inst(in).arity = argCount;
    kindInst(in,head);

    if (nonNull(findInst(inst(in).c,inst(in).t))) {
	ERRMSG(line) "Repeated instance declaration for "
	ETHEN ERRPRED(head);
	ERRTEXT "\n"
	EEND;
    }
    else
	cclass(inst(in).c).instances
	    = appendOnto(cclass(inst(in).c).instances,singleton(in));

    if (nonNull(extractSigdecls(inst(in).implements))) {
        ERRMSG(line) "Type signature decls not permitted in instance decl"
        EEND;
    }
    inst(in).implements = classBindings("instance",
					inst(in).c,
					extractBindings(inst(in).implements));
}

/* --------------------------------------------------------------------------
 * Verifying superclass constraints:
 *
 * Unlike Gofer, the Haskell report requires strict static checks on
 * instance declarations to ensure that superclass hierarchies can be
 * constructed.  The restrictions are outlined on Pages 41--42 of the
 * Haskell 1.3 report.  The effect of these rules is that, for each
 * pair of declarations:
 *
 *    class C a => D a where ...
 *    instance ps => D (T a1 ... an) where ...
 *
 * there must also be an instance:
 *
 *    instance ps1 => C (T a1 ... an) where ...
 *
 * such that ps1 is always implied by ps.  Since Haskell and Hugs restrict
 * these two contexts to predicates of the form Class var, this is equivalent
 * to requiring that each pi' in ps1 is a subclass (not necessarily proper)
 * of some pi in ps.
 * ------------------------------------------------------------------------*/

static Void local checkInstSC(in)	/* check superclass constraints for*/
Inst in; {				/* a given instance, in		   */
    Class c   = inst(in).c;
    List  scs = cclass(c).supers;
    List  ps  = inst(in).specifics;
    Int   n   = dictSpecificsStart(c);

    for (inst(in).superBuild=NIL; nonNull(scs); scs=tl(scs)) {
	Class sc   = hd(scs);
	Inst  scin = findInst(sc,inst(in).t);
	List  ps1  = NIL;

	if (isNull(scin)) {				/* condition 1	   */
	    Cell cpi  = makeInstPred(in);
	    Cell scpi = ap(sc,arg(cpi));
	    ERRMSG(inst(in).line) "Definition of "   ETHEN ERRPRED(cpi);
	    ERRTEXT " requires superclass instance " ETHEN ERRPRED(scpi);
	    ERRTEXT "\n"
	    EEND;
	}

	for (ps1=inst(scin).specifics; nonNull(ps1); ps1=tl(ps1)) {
	    Cell e = scEvidFrom(hd(ps1),ps,n);		/* condition 2	   */
	    if (nonNull(e))
		    scin = ap(scin,e);
	    else {
		Cell cpi  = makeInstPred(in);
		Cell scpi = ap(sc,arg(cpi));
		ERRMSG(inst(in).line) "Cannot build superclass instance "
					     ETHEN ERRPRED(scpi);
		ERRTEXT " of "		     ETHEN ERRPRED(cpi);
		ERRTEXT ":\n*** Context  : " ETHEN ERRCONTEXT(ps);
		ERRTEXT "\n*** Required : "  ETHEN ERRPRED(hd(ps1));
		ERRTEXT "\n"
		EEND;
	    }
	}
	inst(in).superBuild = cons(scin,inst(in).superBuild);
    }
    inst(in).superBuild = rev(inst(in).superBuild);
}

static Cell local scEvidFrom(pi,ps,n)	/* Calculate evidence for pred	   */
Cell pi;				/* pi from ps using superclass	   */
List ps;				/* entailment			   */
Int  n; {
    for (; nonNull(ps); ps=tl(ps), n++)
	if (arg(pi)==arg(hd(ps))) {
	    Cell e = superEvid(mkOffset(n),fun(hd(ps)),fun(pi));
	    if (nonNull(e))
		return e;
	}
    return NIL;
}

/* --------------------------------------------------------------------------
 * Process class and instance declaration binding groups:
 * ------------------------------------------------------------------------*/

static List local classBindings(where,c,bs)
String where;                          /* check validity of bindings bs for*/
Class  c;                              /* class c (or an instance of c)    */
List   bs; {                           /* sort into approp. member order   */
    List nbs = NIL;

    for (; nonNull(bs); bs=tl(bs)) {
	Cell b  = hd(bs);
	Name nm = newName(inventText());   /* pick name for implementation */
	Int  mno;

	if (!isVar(fst(b))) {          /* only allows function bindings    */
	    ERRMSG(rhsLine(snd(snd(snd(b)))))
	       "Pattern binding illegal in %s declaration", where
	    EEND;
	}

	if ((mno=memberNumber(c,textOf(fst(b))))==0) {
	    ERRMSG(rhsLine(snd(hd(snd(snd(b))))))
		"No member \"%s\" in class \"%s\"",
		textToStr(textOf(fst(b))), textToStr(cclass(c).text)
	    EEND;
	}

	name(nm).defn = snd(snd(b));   /* save definition of implementation*/
	nbs           = numInsert(mno-1,nm,nbs);
    }
    return nbs;
}

static Int local memberNumber(c,t)     /* return number of member function */
Class c;                               /* with name t in class c           */
Text  t; {                             /* return 0 if not a member         */
    List ms = cclass(c).members;
    for (; nonNull(ms); ms=tl(ms))
	if (t==name(hd(ms)).text)
	    return mfunOf(hd(ms));
    return 0;
}

static List local numInsert(n,x,xs)    /* insert x at nth position in xs,  */
Int  n;                                /* filling gaps with NIL            */
Cell x;
List xs; {
    List start = isNull(xs) ? cons(NIL,NIL) : xs;

    for (xs=start; 0<n--; xs=tl(xs))
	if (isNull(tl(xs)))
	    tl(xs) = cons(NIL,NIL);
    hd(xs) = x;
    return start;
}

/* --------------------------------------------------------------------------
 * Process derived instance requests:
 * ------------------------------------------------------------------------*/

static List derivedInsts;		/* list of derived instances	   */
static Bool instsChanged;

static Void local checkDerive(t,p,ts,ct)/* verify derived instance request */
Tycon t;				/* for tycon t, with explicit	   */
List  p;				/* context p, component types ts   */
List  ts;				/* and named class ct		   */
Cell  ct; {
    Int   line = tycon(t).line;
    Class c    = findClass(textOf(ct));
    if (isNull(c)) {
	ERRMSG(line) "Unknown class \"%s\" in derived instance",
		     textToStr(textOf(ct))
	EEND;
    }
    addDerInst(line,c,p,dupList(ts),t,tycon(t).arity);
}

static Void local addDerInst(line,c,p,cts,t,a)
Int   line;				/* add a derived instance	  */
Class c;
List  p, cts;
Type  t;
Int   a; {
    Inst in;

    if (nonNull(findInst(c,t))) {
	ERRMSG(line) "Duplicate derived instance for class \"%s\"",
		     textToStr(cclass(c).text)
	EEND;
    }

    p = appendOnto(dupList(p),singleton(NIL));	/* set initial values for  */
#define applyClass(t) ap(c,t)			/* derived instance calc.  */
    mapOver(applyClass,cts);
#undef  applyClass

    in		        = newInst();
    inst(in).c          = c;
    inst(in).t          = t;
    inst(in).arity      = a;
    inst(in).line       = line;
    inst(in).specifics  = ap(DERIVE,pair(p,cts));
    inst(in).implements = NIL;
    cclass(c).instances = appendOnto(cclass(c).instances,singleton(in));
    derivedInsts        = cons(in,derivedInsts);
}

Void addTupInst(c,n)			/* Request derived instance of c   */
Class c;				/* for mkTuple(n) constructor	   */
Int   n; {
    Int  m              = n;
    List cts            = NIL;
    while (0<m--)
	cts = cons(mkOffset(m),cts);
    addDerInst(0,c,NIL,cts,mkTuple(n),n);
}

Void addEvalInst(line,t,arity,ctxt)	/* Add dummy instance for Eval	   */
Int  line;
Cell t;
Int  arity;
List ctxt; {
    if (nonNull(findInst(classEval,t)))
	internal("addEvalInst");
    else {
	Inst in		      = newInst();
	inst(in).c	      = classEval;
	inst(in).t	      = t;
	inst(in).arity	      = arity;
	inst(in).line	      = line;
	inst(in).specifics    = ctxt;
	inst(in).numSpecifics = length(ctxt);
	cclass(classEval).instances
		     = appendOnto(cclass(classEval).instances,singleton(in));
    }
}

static Void local deriveContexts(is)	/* Calculate contexts for derived  */
List is; {				/* instances			   */

    mapProc(addDerivImp,is);		/* First, add implementations and  */
    mapProc(expandDComps,is);		/* expand syns in component types  */

    do {				/* Main calculation of contexts	   */
	instsChanged = FALSE;
	mapProc(calcInstPreds,derivedInsts);
    } while (instsChanged);

    for (; nonNull(is); is=tl(is)) {	/* Extract and simplify results	   */
	inst(hd(is)).specifics
	    = superSimp(initSeg(fst(snd(inst(hd(is)).specifics))));
	inst(hd(is)).numSpecifics = length(inst(hd(is)).specifics);
    }
}

static Void local expandDComps(in)	/* Expand away synonyms appearing  */
Inst in; {				/* in the component types	   */
    List cts = snd(snd(inst(in).specifics));
    for (; nonNull(cts); cts=tl(cts))
	snd(hd(cts)) = fullExpand(snd(hd(cts)));
}

static List local superSimp(ps)		/* Simplify preds in ps using super*/
List ps; {				/* class hierarchy ...		   */
    Int n = length(ps);

    while (0<n--)
	if (nonNull(scEvidFrom(hd(ps),tl(ps),0)))
	    ps = tl(ps);
	else {
	    Cell tmp = tl(ps);
	    tl(ps)   = NIL;
	    ps       = appendOnto(tmp,ps);
	}
    return ps;
}

static Void local maybeAddPred(pi,ps)	/* Add predicate pi to the list ps,*/
Cell pi;				/* setting the instsChanged flag if*/
List ps; {				/* pi is not already a member.	   */
    Class c = fun(pi);
    Cell  v = arg(pi);
    for (; nonNull(ps); ps=tl(ps))
	if (isNull(hd(ps))) {		/* reached the `dummy' end of list?*/
	    hd(ps)       = pi;
	    tl(ps)       = pair(NIL,NIL);
	    instsChanged = TRUE;
	    return;
	}
	else if (fun(hd(ps))==c && arg(hd(ps))==v)
	    return;
}

static Cell local instPred(pi,t)	/* Create instance of Hask pred pi */
Cell pi;				/* under the simple substitution   */
Type t; {				/* represented by t		   */
    return ap(fun(pi),nthArg(offsetOf(arg(pi)),t));
}

static Void local calcInstPreds(in)	/* Calculate next approximation	   */
Inst in; {				/* of the context for a derived	   */
    List retain = NIL;			/* instance			   */
    List ps     = snd(snd(inst(in).specifics));
    List spcs   = fst(snd(inst(in).specifics));

    while (nonNull(ps)) {
	Cell pi = hd(ps);
	ps      = tl(ps);
	if (isClass(fun(pi))) {			/* Class type		   */
	    if (isOffset(arg(pi)))		/* Class variable	   */
		maybeAddPred(pi,spcs);
	    else {				/* Class (T t1 ... tn)	   */
		Class c   = fun(pi);
		Cell  t   = getHead(arg(pi));
		Inst  in1 = findInst(c,t);

		if (isNull(in1)) {		/* No suitable instance	   */
		    Cell bpi = makeInstPred(in);
		    ERRMSG(inst(in).line) "An instance of " ETHEN ERRPRED(pi);
		    ERRTEXT " is required to derive "      ETHEN ERRPRED(bpi);
		    ERRTEXT "\n"
		    EEND;
		}				/* previously defined inst */
		else if (whatIs(inst(in1).specifics)!=DERIVE) {
		    List qs = inst(in1).specifics;
		    for (; nonNull(qs); qs=tl(qs))
			ps = cons(instPred(hd(qs),arg(pi)),ps);
		}
		else {				/* still being derived	   */
		    List qs = fst(snd(inst(in1).specifics));
		    for (; nonNull(hd(qs)); qs=tl(qs))
			ps = cons(instPred(hd(qs),arg(pi)),ps);
		    retain = cons(pair(arg(pi),qs),retain);
		    instsChanged = TRUE;
		}
	    }
	}
	else {					/* Application of a subst  */
	    List qs = snd(pi);			/* to a list of predicates,*/
	    if (nonNull(hd(qs)))		/* given by a variable	   */
		instsChanged = TRUE;
	    for (; nonNull(hd(qs)); qs=tl(qs))
		ps = cons(instPred(hd(qs),fst(pi)),ps);
	    retain = cons(pair(fst(pi),qs),retain);
	}
    }

    snd(snd(inst(in).specifics)) = retain;
}

/* --------------------------------------------------------------------------
 * Generate code for derived instances:
 * ------------------------------------------------------------------------*/

static Void local addDerivImp(in)
Inst in; {
    List imp = NIL;
    if (inst(in).c==classEq)
	imp = deriveEq(inst(in).t);
    else if (inst(in).c==classOrd)
	imp = deriveOrd(inst(in).t);
    else if (inst(in).c==classEnum)
	imp = deriveEnum(inst(in).t);
    else if (inst(in).c==classIx)
	imp = deriveIx(inst(in).t);
    else if (inst(in).c==classShow)
	imp = deriveShow(inst(in).t);
    else if (inst(in).c==classRead)
	imp = deriveRead(inst(in).t);
    else if (inst(in).c==classBounded)
	imp = deriveBounded(inst(in).t);
    else {
	ERRMSG(inst(in).line) "Cannot derive instances of class \"%s\"",
			      textToStr(cclass(inst(in).c).text)
	EEND;
    }

    inst(in).implements = classBindings("derived instance",
					inst(in).c,
					imp);
}

static List diVars = NIL;		/* Acts as a cache of invented vars*/
static Int  diNum  = 0;

static List local getDiVars(n)		/* get list of at least n vars for */
Int n; {				/* derived instance generation	   */
    for (; diNum<n; diNum++)
	diVars = cons(inventVar(),diVars);
    return diVars;
}

static Cell local mkBind(s,alts)	/* make a binding for a variable   */
String s;
List   alts; {
    return pair(mkVar(findText(s)),pair(NIL,alts));
}

static Cell local mkVarAlts(line,r)	/* make alts for binding a var to  */
Int  line;				/* a simple expression		   */
Cell r; {
    return singleton(pair(NIL,pair(mkInt(line),r)));
}

/* --------------------------------------------------------------------------
 * Given a datatype:   data T a b = A a b | B Int | C  deriving (Eq, Ord)
 * The derived definitions of equality and ordering are given by:
 *
 *   A a b == A x y  =  a==x && b==y
 *   B a   == B x    =  a==x
 *   C     == C      =  True
 *   _     == _      =  False
 *
 *   compare (A a b) (A x y) =  primCompAux a x (compare b y)
 *   compare (B a)   (B x)   =  compare a x
 *   compare C       C       =  EQ
 *   compare a       x       =  cmpConstr a x
 *
 * In each case, the last line is only needed if there are multiple
 * constructors in the datatype definition.
 * ------------------------------------------------------------------------*/

#define ap2(f,x,y) ap(ap(f,x),y)

static List local deriveEq(t)		/* generate binding for derived == */
Type t; {				/* for some TUPLE or DATATYPE t	   */
    List alts = NIL;
    if (isTycon(t)) {			/* deal with type constrs	   */
	List cs = tycon(t).defn;
	for (; hasCfun(cs); cs=tl(cs))
	    alts = cons(mkAltEq(tycon(t).line,
				makeDPats2(hd(cs),name(hd(cs)).arity)),
			alts);
	if (cfunOf(hd(tycon(t).defn))!=0)
	    alts = cons(pair(cons(WILDCARD,cons(WILDCARD,NIL)),
			     pair(mkInt(tycon(t).line),nameFalse)),alts);
	alts = rev(alts);
    }
    else				/* special case for tuples	   */
	alts = singleton(mkAltEq(0,makeDPats2(t,tupleOf(t))));

    return singleton(mkBind("==",alts));
}

static Pair local mkAltEq(line,pats)	/* make alt for an equation for == */
Int  line;				/* using patterns in pats for lhs  */
List pats; {				/* arguments			   */
    Cell p = hd(pats);
    Cell q = hd(tl(pats));
    Cell e = nameTrue;

    if (isAp(p)) {
	e = ap2(nameEq,arg(p),arg(q));
	for (p=fun(p), q=fun(q); isAp(p); p=fun(p), q=fun(q))
	    e = ap2(nameAnd,ap2(nameEq,arg(p),arg(q)),e);
    }
    return pair(pats,pair(mkInt(line),e));
}

static List local deriveOrd(t)		/* make binding for derived compare*/
Type t; {				/* for some TUPLE or DATATYPE t	   */
    List alts = NIL;
    if (isEnumType(t))			/* special case for enumerations   */
	alts = mkVarAlts(tycon(t).line,nameConCmp);
    else if (isTycon(t)) {		/* deal with type constrs	   */
	List cs = tycon(t).defn;
	for (; hasCfun(cs); cs=tl(cs))
	    alts = cons(mkAltOrd(tycon(t).line,
				 makeDPats2(hd(cs),name(hd(cs)).arity)),
			alts);
	if (cfunOf(hd(tycon(t).defn))!=0) {
	    Cell u = inventVar();
	    Cell w = inventVar();
	    alts   = cons(pair(cons(u,singleton(w)),
			       pair(mkInt(tycon(t).line),
				    ap2(nameConCmp,u,w))),alts);
	}
	alts = rev(alts);
    }
    else				/* special case for tuples	   */
	alts = singleton(mkAltOrd(0,makeDPats2(t,tupleOf(t))));

    return singleton(mkBind("compare",alts));
}

static Pair local mkAltOrd(line,pats)	/* make alt for eqn for compare	   */
Int  line;				/* using patterns in pats for lhs  */
List pats; {				/* arguments			   */
    Cell p = hd(pats);
    Cell q = hd(tl(pats));
    Cell e = nameEQ;

    if (isAp(p)) {
	e = ap2(nameCompare,arg(p),arg(q));
	for (p=fun(p), q=fun(q); isAp(p); p=fun(p), q=fun(q))
	    e = ap(ap2(nameCompAux,arg(p),arg(q)),e);
    }

    return pair(pats,pair(mkInt(line),e));
}

static List local makeDPats2(h,n)	/* generate pattern list	   */
Cell h;					/* by putting two new patterns with*/
Int  n; {				/* head h and new var components   */
    List us = getDiVars(2*n);
    List vs = NIL;
    Cell p;
    Int  i;

    for (i=0, p=h; i<n; ++i) {		/* make first version of pattern   */
	p  = ap(p,hd(us));
	us = tl(us);
    }
    vs = cons(p,vs);

    for (i=0, p=h; i<n; ++i) {		/* make second version of pattern  */
	p  = ap(p,hd(us));
	us = tl(us);
    }
    return cons(p,vs);
}

/* --------------------------------------------------------------------------
 * Deriving Ix and Enum:
 * ------------------------------------------------------------------------*/

static List local deriveEnum(t)	/* Construct definition of enumeration	   */
Tycon t; {
    Int l = tycon(t).line;

    if (!isEnumType(t)) {
	ERRMSG(l) "Can only derive instances of Enum for enumeration types"
	EEND;
    }

    return cons(mkBind("toEnum",mkVarAlts(l,ap(nameEnToEn,hd(tycon(t).defn)))),
	    cons(mkBind("fromEnum",mkVarAlts(l,nameEnFrEn)),
	     cons(mkBind("enumFrom",mkVarAlts(l,nameEnFrom)),
	      cons(mkBind("enumFromTo",mkVarAlts(l,nameEnFrTo)),
	       cons(mkBind("enumFromThen",mkVarAlts(l,nameEnFrTh)),NIL)))));
}

static List local deriveIx(t)	/* Construct definition of indexing	   */
Tycon t; {
    if (isEnumType(t))		/* Definitions for enumerations		   */
	return cons(mkBind("range",mkVarAlts(tycon(t).line,nameEnRange)),
		cons(mkBind("index",mkVarAlts(tycon(t).line,nameEnIndex)),
		 cons(mkBind("inRange",mkVarAlts(tycon(t).line,nameEnInRng)),
		  NIL)));
    else if (isTuple(t))	/* Definitions for product types	   */
	return mkIxBinds(0,t,tupleOf(t));
    else if (isTycon(t) && cfunOf(hd(tycon(t).defn))==0)
	return mkIxBinds(tycon(t).line,
			 hd(tycon(t).defn),
			 name(hd(tycon(t).defn)).arity);

    ERRMSG(tycon(t).line)
	"Can only derive instances of Ix for enumeration or product types"
    EEND;
    return NIL;/* NOTREACHED*/
}

static Bool local isEnumType(t)	/* Determine whether t is an enumeration   */
Tycon t; {			/* type (i.e. all constructors arity == 0) */
    if (isTycon(t) && (tycon(t).what==DATATYPE || tycon(t).what==NEWTYPE)) {
	List cs = tycon(t).defn;
	for (; hasCfun(cs); cs=tl(cs))
	    if (name(hd(cs)).arity!=0)
		return FALSE;
	addCfunTable(t);
	return TRUE;
    }
    return FALSE;
}

static List local mkIxBinds(line,h,n)	/* build bindings for derived Ix on*/
Int  line;				/* a product type		   */
Cell h;
Int  n; {
    List vs   = getDiVars(3*n);
    Cell ls   = h;
    Cell us   = h;
    Cell is   = h;
    Cell pr   = NIL;
    Cell pats = NIL;
    Int  i;

    for (i=0; i<n; ++i, vs=tl(vs)) {	/* build three patterns for values */
	ls = ap(ls,hd(vs));		/* of the datatype concerned	   */
	us = ap(us,hd(vs=tl(vs)));
	is = ap(is,hd(vs=tl(vs)));
    }
    pr   = ap2(mkTuple(2),ls,us);	/* Build (ls,us)		   */
    pats = cons(pr,cons(is,NIL));	/* Build [(ls,us),is]		   */

    return cons(prodRange(line,singleton(pr),ls,us,is),
		cons(prodIndex(line,pats,ls,us,is),
		     cons(prodInRange(line,pats,ls,us,is),NIL)));
}

static Cell local prodRange(line,pats,ls,us,is)
Int  line;				/* Make definition of range for a  */
List pats;				/* product type			   */
Cell ls, us, is; {
    /* range :: (a,a) -> [a]
     * range (X a b c, X p q r)
     *   = [ X x y z | x <- range (a,p), y <- range (b,q), z <- range (c,r) ]
     */
    Cell is1 = is;
    List e   = NIL;
    for (; isAp(ls); ls=fun(ls), us=fun(us), is=fun(is))
	e = cons(ap(FROMQUAL,pair(arg(is),
				  ap(nameRange,ap2(mkTuple(2),
						   arg(ls),
						   arg(us))))),e);
    e = ap(COMP,pair(is1,e));
    e = singleton(pair(pats,pair(mkInt(line),e)));
    return mkBind("range",e);
}

static Cell local prodIndex(line,pats,ls,us,is)
Int  line;				/* Make definition of index for a  */
List pats;				/* product type			   */
Cell ls, us, is; {
    /* index :: (a,a) -> a -> Bool
     * index (X a b c, X p q r) (X x y z)
     *  = index (c,r) z + rangeSize (c,r) * (
     *     index (b,q) y + rangeSize (b,q) * (
     *      index (a,x) x))
     */
    List xs = NIL;
    Cell e  = NIL;
    for (; isAp(ls); ls=fun(ls), us=fun(us), is=fun(is))
	xs = cons(ap2(nameIndex,ap2(mkTuple(2),arg(ls),arg(us)),arg(is)),xs);
    for (e=hd(xs); nonNull(xs=tl(xs));) {
	Cell x = hd(xs);
	e = ap2(namePlus,x,ap2(nameMult,ap(nameRangeSize,arg(fun(x))),e));
    }
    e = singleton(pair(pats,pair(mkInt(line),e)));
    return mkBind("index",e);
}

static Cell local prodInRange(line,pats,ls,us,is)
Int  line;				/* Make definition of inRange for a*/
List pats;				/* product type			   */
Cell ls, us, is; {
    /* inRange :: (a,a) -> a -> Bool
     * inRange (X a b c, X p q r) (X x y z)
     *          = inRange (a,p) x && inRange (b,q) y && inRange (c,r) z
     */
    Cell e = ap2(nameInRange,ap2(mkTuple(2),arg(ls),arg(us)),arg(is));
    while (ls=fun(ls), us=fun(us), is=fun(is), isAp(ls))
	e = ap2(nameAnd,
		ap2(nameInRange,ap2(mkTuple(2),arg(ls),arg(us)),arg(is)),
		e);
    e = singleton(pair(pats,pair(mkInt(line),e)));
    return mkBind("inRange",e);
}

/* --------------------------------------------------------------------------
 * Deriving Show:
 * ------------------------------------------------------------------------*/

static List local deriveShow(t)	/* Construct definition of text conversion */
Tycon t; {
    List alts = NIL;
    if (isTycon(t)) {			/* deal with type constrs	   */
	List cs = tycon(t).defn;
	for (; hasCfun(cs); cs=tl(cs))
	    alts = cons(mkAltShow(tycon(t).line,hd(cs),name(hd(cs)).arity),
			alts);
	alts = rev(alts);
    }
    else				/* special case for tuples	   */
	alts = singleton(mkAltShow(0,t,tupleOf(t)));

    return singleton(mkBind("showsPrec",alts));
}

static Cell local mkAltShow(line,h,a)	/* make alt for showsPrec eqn	   */
Int  line;
Cell h;
Int  a; {
    List vs   = getDiVars(a+1);
    Cell d    = hd(vs);
    Cell pat  = h;
    List pats = NIL;
    while (vs=tl(vs), 0<a--)
	pat = ap(pat,hd(vs));
    pats = cons(d,cons(pat,NIL));
    return pair(pats,pair(mkInt(line),showsPrecRhs(d,pat)));
}

#define shows0   ap(nameShowsPrec,mkInt(0))
#define shows10  ap(nameShowsPrec,mkInt(10))
#define showsOP  ap(nameComp,consChar('('))
#define showsOB  ap(nameComp,consChar('{'))
#define showsCM  ap(nameComp,consChar(','))
#define showsSP  ap(nameComp,consChar(' '))
#define showsBQ  ap(nameComp,consChar('`'))
#define showsCP  consChar(')')
#define showsCB  consChar('}')

static Cell local showsPrecRhs(d,pat)	/* build a rhs for showsPrec for a */
Cell d, pat; {				/* given pattern, pat		   */
    Cell h   = getHead(pat);
    List cfs = cfunSfuns;

    if (isTuple(h)) {
	/* To display a tuple:
	 *    showsPrec d (a,b,c,d) = showChar '(' . showsPrec 0 a .
	 *			      showChar ',' . showsPrec 0 b .
	 *			      showChar ',' . showsPrec 0 c .
	 *			      showChar ',' . showsPrec 0 d .
	 *			      showChar ')'
	 */
	Int  i   = tupleOf(h);
	Cell rhs = showsCP;
	for (; i>1; --i) {
            rhs = ap(showsCM,ap2(nameComp,ap(shows0,arg(pat)),rhs));
	    pat = fun(pat);
	}
	return ap(showsOP,ap2(nameComp,ap(shows0,arg(pat)),rhs));
    }

    for (; nonNull(cfs) && h!=fst(hd(cfs)); cfs=tl(cfs))
	;
    if (nonNull(cfs)) {
	/* To display a value using record syntax:
	 *    showsPrec d C{x=e, y=f, z=g} = showString "C"  . showChar '{' .
	 *				     showField "x" e . showChar ',' .
	 *				     showField "y" f . showChar ',' .
	 *				     showField "z" g . showChar '}'
	 *    showField lab val
	 *	= showString lab . showChar '=' . shows val
	 */
	Cell rhs     = showsCB;
	List vs      = rev(snd(hd(cfs)));
	snd(hd(cfs)) = NIL;
	if (isAp(pat)) {
	    for (;;) {
		rhs = ap2(nameComp,
			  ap2(nameShowField,
			      mkStr(textOf(hd(vs))),
			      arg(pat)),
			  rhs);
		pat = fun(pat);
		vs  = tl(vs);
		if (isAp(pat))
		    rhs = ap(showsCM,rhs);
		else
		    break;
	    }
	}
	rhs = ap2(nameComp,ap(nameApp,mkStr(name(h).text)),ap(showsOB,rhs));
	return rhs;
    }
    else if (name(h).arity==0)
	/* To display a nullary constructor:
	 *    showsPrec d Foo = showString "Foo"
	 */
	return ap(nameApp,mkStr(name(h).text));
    else {
	Syntax s = syntaxOf(name(h).text);
	if (name(h).arity==2 && assocOf(s)!=APPLIC) {
	    /* For a binary constructor with prec p:
	     * showsPrec d (a :* b) = showParen (d > p)
	     *				(showsPrec lp a . showChar ' ' .
	     *				 showsString s  . showChar ' ' .
	     *				 showsPrec rp b)
	     */
	    Int  p   = precOf(s);
	    Int  lp  = (assocOf(s)==LEFT_ASS)  ? p : (p+1);
	    Int  rp  = (assocOf(s)==RIGHT_ASS) ? p : (p+1);
	    Cell rhs = ap(showsSP,ap2(nameShowsPrec,mkInt(rp),arg(pat)));
	    if (defaultSyntax(name(h).text)==APPLIC)
		rhs = ap(showsBQ,
			 ap2(nameComp,
			     ap(nameApp,mkStr(name(h).text)),
			     ap(showsBQ,rhs)));
	    else
		rhs = ap2(nameComp,ap(nameApp,mkStr(name(h).text)),rhs);

	    rhs = ap2(nameComp,
		      ap2(nameShowsPrec,mkInt(lp),arg(fun(pat))),
		      ap(showsSP,rhs));
	    rhs = ap2(nameShowParen,ap2(nameLe,mkInt(p+1),d),rhs);
	    return rhs;
	}
	else {
	    /* To display a non-nullary constructor with applicative syntax:
	     *    showsPrec d (Foo x y) = showParen (d>=10)
	     *				   (showString "Foo" .
	     *				    showChar ' ' . showsPrec 10 x .
	     *				    showChar ' ' . showsPrec 10 y)
	     */
	    Cell rhs = ap(showsSP,ap(shows10,arg(pat)));
	    for (pat=fun(pat); isAp(pat); pat=fun(pat))
		rhs = ap(showsSP,ap2(nameComp,ap(shows10,arg(pat)),rhs));
	    rhs = ap2(nameComp,ap(nameApp,mkStr(name(h).text)),rhs);
	    rhs = ap2(nameShowParen,ap2(nameLe,mkInt(10),d),rhs);
	    return rhs;
	}
    }
}
#undef  shows10
#undef  shows0
#undef  showsOP
#undef  showsOB
#undef  showsCM
#undef  showsSP
#undef  showsBQ
#undef  showsCP
#undef  showsCB

/* --------------------------------------------------------------------------
 * Deriving Read:
 * ------------------------------------------------------------------------*/

static List local deriveRead(t)	/* construct definition of text reader	   */
Tycon t; {
    return NIL;			/* NOT YET IMPLEMENTED			   */
}

/* --------------------------------------------------------------------------
 * Deriving Bounded:
 * ------------------------------------------------------------------------*/

static List local deriveBounded(t)/* construct definition of bounds	   */
Tycon t; {
    if (isEnumType(t)) {
	Cell last  = tycon(t).defn;
	Cell first = hd(last);
	while (hasCfun(tl(last)))
	    last = tl(last);
	return cons(mkBind("minBound",mkVarAlts(tycon(t).line,first)),
		cons(mkBind("maxBound",mkVarAlts(tycon(t).line,hd(last))),
		 NIL));
    }
    else if (isTuple(t))	/* Definitions for product types	   */
	return mkBndBinds(0,t,tupleOf(t));
    else if (isTycon(t) && cfunOf(hd(tycon(t).defn))==0)
	return mkBndBinds(tycon(t).line,
			  hd(tycon(t).defn),
			  name(hd(tycon(t).defn)).arity);

    ERRMSG(tycon(t).line)
     "Can only derive instances of Bounded for enumeration and product types"
    EEND;
    return NIL;
}

static List local mkBndBinds(line,h,n)	/* build bindings for derived	   */
Int  line;				/* Bounded on a product type	   */
Cell h;
Int  n; {
    Cell minB = h;
    Cell maxB = h;
    while (n-- > 0) {
	minB = ap(minB,nameMinBnd);
	maxB = ap(maxB,nameMaxBnd);
    }
    return cons(mkBind("minBound",mkVarAlts(line,minB)),
	    cons(mkBind("maxBound",mkVarAlts(line,maxB)),
	     NIL));
}

/* --------------------------------------------------------------------------
 * Primitive definitions are usually only included in the first script
 * file read - the prelude.  A primitive definition associates a variable
 * name with a string (which identifies a built-in primitive) and a type.
 * ------------------------------------------------------------------------*/

Void primDefn(line,prims,type)		/* Handle primitive definitions	   */
Cell line;
List prims;
Cell type; {
    primDefns = cons(triple(line,prims,type),primDefns);
}

static Void local checkPrimDefn(p)	/* Check primitive definition	   */
Triple p; {
    Int  line  = intOf(fst3(p));
    List prims = snd3(p);
    Type type  = thd3(p);
    type = checkSigType(line,"primitive definition",fst(hd(prims)),type);
    for (; nonNull(prims); prims=tl(prims)) {
	Cell   p    = hd(prims);
	Bool   same = isVar(p);
	Text   pt   = textOf(same ? p : fst(p));
	String pr   = textToStr(textOf(same ? p : snd(p)));
	addNewPrim(line,pt,pr,type);
    }
}

static Void local addNewPrim(l,vn,s,t)	/* make binding of variable vn to  */
Int    l;				/* primitive function referred	   */
Text   vn;				/* to by s, with given type t	   */
String s;
Cell   t;{
    Name n = findName(vn);

    if (isNull(n))
	n = newName(vn);
    else if (name(n).defn!=PREDEFINED) {
	ERRMSG(l) "Redeclaration of primitive \"%s\"", textToStr(vn)
	EEND;
    }

    addPrim(l,n,s,t);
}

/* --------------------------------------------------------------------------
 * Default definitions; only one default definition is permitted in a
 * given script file.  If no default is supplied, then a standard system
 * default will be used where necessary.
 * ------------------------------------------------------------------------*/

Void defaultDefn(line,defs)		/* Handle default types definition */
Int  line;
List defs; {
    if (defaultLine!=0) {
        ERRMSG(line) "Multiple default declarations are not permitted in" ETHEN
        ERRTEXT     "a single script file.\n"
	EEND;
    }
    defaultDefns = defs;
    defaultLine  = line;
}

static Void local checkDefaultDefns() {	/* check that default types are	   */
    List  ds = NIL;			/* well-kinded instances of Num	   */

    if (defaultLine!=0) {
	map2Over(depTypeExp,defaultLine,NIL,defaultDefns);
	kindDefaults(defaultLine,defaultDefns);
	mapOver(fullExpand,defaultDefns);
    }
    else
	defaultDefns = stdDefaults;

    if (isNull(classNum))
	classNum = findClass(findText("Num"));

    for (ds=defaultDefns; nonNull(ds); ds=tl(ds))
	if (!mtInst(classNum,hd(ds))) {
	    ERRMSG(defaultLine)
		"Default types must be instances of the Num class"
	    EEND;
	}
}

/* --------------------------------------------------------------------------
 * Static analysis of patterns:
 *
 * Patterns are parsed as ordinary (atomic) expressions.  Static analysis
 * makes the following checks:
 *  - Patterns are well formed (according to pattern syntax), including the
 *    special case of (n+k) patterns.
 *  - All constructor functions have been defined and are used with the
 *    correct number of arguments.
 *  - No variable name is used more than once in a pattern.
 *
 * The list of pattern variables occuring in each pattern is accumulated in
 * a global list `patVars', which must be initialised to NIL at appropriate
 * points before using these routines to check for valid patterns.  This
 * mechanism enables the pattern checking routine to be mapped over a list
 * of patterns, ensuring that no variable occurs more than once in the
 * complete pattern list (as is required on the lhs of a function defn).
 * ------------------------------------------------------------------------*/

static List patVars;		       /* list of vars bound in pattern    */

static Cell local checkPat(line,p)     /* Check valid pattern syntax	   */
Int  line;
Cell p; {
    switch (whatIs(p)) {
	case VARIDCELL :
	case VAROPCELL : addPatVar(line,p);
			 break;

	case AP        : return checkMaybeCnkPat(line,p);

	case NAME      :
	case CONIDCELL :
	case CONOPCELL : return checkApPat(line,0,p);

#if BIGNUMS
	case ZERONUM   :
	case POSNUM    :
	case NEGNUM    :
#endif
	case WILDCARD  :
	case STRCELL   :
	case CHARCELL  :
	case INTCELL   : break;

	case ASPAT     : addPatVar(line,fst(snd(p)));
			 snd(snd(p)) = checkPat(line,snd(snd(p)));
			 break;

	case LAZYPAT   : snd(p) = checkPat(line,snd(p));
			 break;

	case FINLIST   : map1Over(checkPat,line,snd(p));
			 break;

	case CONFLDS   : depConFlds(line,p,TRUE);
			 break;

	default        : ERRMSG(line) "Illegal pattern syntax"
			 EEND;
    }
    return p;
}

static Cell local checkMaybeCnkPat(l,p)/* Check applicative pattern with   */
Int  l;				       /* the possibility of n+k pattern   */
Cell p; {
#if NPLUSK
    Cell h = getHead(p);

    if (argCount==2 && isVar(h) && textOf(h)==textPlus) {	/* n+k	   */
	Cell v = arg(fun(p));
	if (!isInt(arg(p))) {
		ERRMSG(l) "Second argument in (n+k) pattern must be an integer"
		EEND;
	}
	if (intOf(arg(p))<=0) {
		ERRMSG(l) "Integer k in (n+k) pattern must be > 0"
		EEND;
	}
	fst(fun(p))	 = ADDPAT;
	intValOf(fun(p)) = intOf(arg(p));
	arg(p)		 = checkPat(l,v);
	return p;
    }
#endif
    return checkApPat(l,0,p);
}

static Cell local checkApPat(line,args,p)
Int  line;			       /* check validity of application    */
Int  args;			       /* of constructor to arguments	   */
Cell p; {
    switch (whatIs(p)) {
	case AP        : fun(p) = checkApPat(line,args+1,fun(p));
			 arg(p) = checkPat(line,arg(p));
			 break;

	case TUPLE     : if (tupleOf(p)!=args) {
			     ERRMSG(line) "Illegal tuple pattern"
			     EEND;
			 }
			 break;

	case CONIDCELL :
	case CONOPCELL : p = conDefined(line,textOf(p));
			 checkCfunArgs(line,p,args);
			 break;

	case NAME      : checkIsCfun(line,p);
			 checkCfunArgs(line,p,args);
			 break;

	default        : ERRMSG(line) "Illegal pattern syntax"
			 EEND;
    }
    return p;
}

static Void local addPatVar(line,v)    /* add variable v to list of vars   */
Int  line;			       /* in current pattern, checking for */
Cell v; {			       /* repeated variables.		   */
     Text t = textOf(v);
     List p = NIL;
     List n = patVars;

     for (; nonNull(n); p=n, n=tl(n))
	 if (textOf(hd(n))==t) {
	     ERRMSG(line) "Repeated variable \"%s\" in pattern",
			  textToStr(t)
	     EEND;
	 }

     if (isNull(p))
	 patVars = cons(v,NIL);
     else
	 tl(p)	 = cons(v,NIL);
}

static Name local conDefined(line,t)   /* check that t is the name of a    */
Int line;			       /* previously defined constructor   */
Text t; {			       /* function.			   */
    Cell c=findName(t);
    if (isNull(c)) {
	ERRMSG(line) "Undefined constructor function \"%s\"", textToStr(t)
	EEND;
    }
    checkIsCfun(line,c);
    return c;
}

static Void local checkIsCfun(line,c)  /* Check that c is a constructor fn */
Int  line;
Name c; {
    if (!isCfun(c)) {
	ERRMSG(line) "\"%s\" is not a constructor function",
		     textToStr(name(c).text)
	EEND;
    }
}

static Void local checkCfunArgs(line,c,args)
Int  line;			       /* Check constructor applied with   */
Cell c; 			       /* correct number of arguments	   */
Int  args; {
    if (name(c).arity!=args) {
	ERRMSG(line) "Constructor function \"%s\" needs %d args in pattern",
		     textToStr(name(c).text), name(c).arity
	EEND;
    }
}

/* --------------------------------------------------------------------------
 * Maintaining lists of bound variables and local definitions, for
 * dependency and scope analysis.
 * ------------------------------------------------------------------------*/

static List bounds;		       /* list of lists of bound vars	   */
static List bindings;		       /* list of lists of binds in scope  */
static List depends;		       /* list of lists of dependents	   */

#define saveBvars()	 hd(bounds)    /* list of bvars in current scope   */
#define restoreBvars(bs) hd(bounds)=bs /* restore list of bound variables  */

static Cell local bindPat(line,p)      /* add new bound vars for pattern   */
Int  line;
Cell p; {
    patVars    = NIL;
    p	       = checkPat(line,p);
    hd(bounds) = revOnto(patVars,hd(bounds));
    return p;
}

static Void local bindPats(line,ps)    /* add new bound vars for patterns  */
Int  line;
List ps; {
    patVars    = NIL;
    map1Over(checkPat,line,ps);
    hd(bounds) = revOnto(patVars,hd(bounds));
}

/* --------------------------------------------------------------------------
 * Before processing value and type signature declarations, all data and
 * type definitions have been processed so that:
 * - all valid type constructors (with their arities) are known.
 * - all valid constructor functions (with their arities and types) are
 *   known.
 *
 * The result of parsing a list of value declarations is a list of Eqns:
 *	 Eqn ::= (SIGDECL,(Line,[Var],type))  |  (Expr,Rhs)
 * The ordering of the equations in this list is the reverse of the original
 * ordering in the script parsed.  This is a consequence of the structure of
 * the parser ... but also turns out to be most convenient for the static
 * analysis.
 *
 * As the first stage of the static analysis of value declarations, each
 * list of Eqns is converted to a list of Bindings.  As part of this
 * process:
 * - The ordering of the list of Bindings produced is the same as in the
 *   original script.
 * - When a variable (function) is defined over a number of lines, all
 *   of the definitions should appear together and each should give the
 *   same arity to the variable being defined.
 * - No variable can have more than one definition.
 * - For pattern bindings:
 *   - Each lhs is a valid pattern/function lhs, all constructor functions
 *     have been defined and are used with the correct number of arguments.
 *   - Each lhs contains no repeated pattern variables.
 *   - Each equation defines at least one variable (e.g. True = False is
 *     not allowed).
 * - Types appearing in type signatures are well formed:
 *    - Type constructors used are defined and used with correct number
 *	of arguments.
 *    - type variables are replaced by offsets, type constructor names
 *	by Tycons.
 * - Every variable named in a type signature declaration is defined by
 *   one or more equations elsewhere in the script.
 * - No variable has more than one type declaration.
 *
 * ------------------------------------------------------------------------*/

#define bindingType(b) fst(snd(b))     /* type (or types) for binding	   */
#define fbindAlts(b)   snd(snd(b))     /* alternatives for function binding*/

static List local extractSigdecls(es)  /* extract the SIGDECLS from list   */
List es; {			       /* of equations			   */
    List sigDecls  = NIL;	       /* :: [(Line,[Var],Type)]	   */

    for(; nonNull(es); es=tl(es))
	if (fst(hd(es))==SIGDECL)		     /* type-declaration?  */
	    sigDecls = cons(snd(hd(es)),sigDecls);   /* discard SIGDECL tag*/

    return sigDecls;
}

static List local extractBindings(es)  /* extract untyped bindings from    */
List es; {			       /* given list of equations	   */
    Cell lastVar   = NIL;	       /* = var def'd in last eqn (if any) */
    Int  lastArity = 0; 	       /* = number of args in last defn    */
    List bs	   = NIL;	       /* :: [Binding]			   */

    for(; nonNull(es); es=tl(es)) {
	Cell e = hd(es);

	if (fst(e)!=SIGDECL) {
	    Int  line	 = rhsLine(snd(e));
	    Cell lhsHead = getHead(fst(e));

	    switch (whatIs(lhsHead)) {
		case VARIDCELL :
		case VAROPCELL : {		      /* function-binding? */
		    Cell newAlt = pair(getArgs(fst(e)), snd(e));
		    if (nonNull(lastVar) && textOf(lhsHead)==textOf(lastVar)) {
			if (argCount!=lastArity) {
			    ERRMSG(line)
				"Equations give different arities for \"%s\"",
				textToStr(textOf(lhsHead))
			    EEND;
			}
			fbindAlts(hd(bs)) = cons(newAlt,fbindAlts(hd(bs)));
		    }
		    else {
			lastVar   = lhsHead;
			lastArity = argCount;
			notDefined(line,bs,lhsHead);
			bs	  = cons(pair(lhsHead,
					      pair(NIL,
						   singleton(newAlt))),
					 bs);
		    }
		}
		break;

		case CONFLDS   :
		case CONOPCELL :
		case CONIDCELL :
		case FINLIST   :
		case TUPLE     :
		case NAME      :
		case ASPAT     : lastVar = NIL;       /* pattern-binding?  */
				 patVars = NIL;
				 fst(e)  = checkPat(line,fst(e));
				 if (isNull(patVars)) {
				     ERRMSG(line)
				       "No variables defined in lhs pattern"
				     EEND;
				 }
				 map2Proc(notDefined,line,bs,patVars);
				 bs = cons(pair(patVars,pair(NIL,e)),bs);
				 break;

		default        : ERRMSG(line) "Improper left hand side"
				 EEND;
	    }
	}
    }
    return bs;
}

static List local eqnsToBindings(es)   /* Convert list of equations to list*/
List es; {			       /* of typed bindings		   */
    List bs = extractBindings(es);
    map1Proc(addSigDecl,bs,extractSigdecls(es));
    return bs;
}

static Void local notDefined(line,bs,v)/* check if name already defined in */
Int  line;			       /* list of bindings		   */
List bs;
Cell v; {
    if (nonNull(findBinding(textOf(v),bs))) {
	ERRMSG(line) "\"%s\" multiply defined", textToStr(textOf(v))
	EEND;
    }
}

static Cell local findBinding(t,bs)    /* look for binding for variable t  */
Text t; 			       /* in list of bindings bs	   */
List bs; {
    for (; nonNull(bs); bs=tl(bs))
	if (isVar(fst(hd(bs)))) {		      /* function-binding? */
	    if (textOf(fst(hd(bs)))==t)
		return hd(bs);
	}
	else if (nonNull(varIsMember(t,fst(hd(bs))))) /* pattern-binding?  */
	    return hd(bs);
    return NIL;
}

static Void local addSigDecl(bs,sigDecl)/* add type information to bindings*/
List bs;			       /* :: [Binding]			   */
Cell sigDecl; { 		       /* :: (Line,[Var],Type)		   */
    Int  line = intOf(fst3(sigDecl));
    Cell vs   = snd3(sigDecl);
    Cell type = checkSigType(line,"type declaration",hd(vs),thd3(sigDecl));

    map3Proc(setType,line,type,bs,vs);
}

static Void local setType(line,type,bs,v)
Int  line;			       /* Set type of variable		   */
Cell type;
Cell v;
List bs; {
    Text t = textOf(v);
    Cell b = findBinding(t,bs);

    if (isNull(b)) {
	ERRMSG(line) "Type declaration for variable \"%s\" with no body",
		     textToStr(t)
	EEND;
    }

    if (isVar(fst(b))) {			      /* function-binding? */
	if (isNull(bindingType(b))) {
	    bindingType(b) = type;
	    return;
	}
    }
    else {					      /* pattern-binding?  */
	List vs = fst(b);
	List ts = bindingType(b);

	if (isNull(ts))
	    bindingType(b) = ts = copy(length(vs),NIL);

	while (nonNull(vs) && t!=textOf(hd(vs))) {
	    vs = tl(vs);
	    ts = tl(ts);
	}

	if (nonNull(vs) && isNull(hd(ts))) {
	    hd(ts) = type;
	    return;
	}
    }

    ERRMSG(line) "Repeated type declaration for \"%s\"", textToStr(t)
    EEND;
}

/* --------------------------------------------------------------------------
 * To facilitate dependency analysis, lists of bindings are temporarily
 * augmented with an additional field, which is used in two ways:
 * - to build the `adjacency lists' for the dependency graph. Represented by
 *   a list of pointers to other bindings in the same list of bindings.
 * - to hold strictly positive integer values (depth first search numbers) of
 *   elements `on the stack' during the strongly connected components search
 *   algorithm, or a special value mkInt(0), once the binding has been added
 *   to a particular strongly connected component.
 *
 * Using this extra field, the type of each list of declarations during
 * dependency analysis is [Binding'] where:
 *
 *    Binding' ::= (Var, (Dep, (Type, [Alt])))	      -- function binding
 *		|  ([Var], (Dep, (Type, (Pat,Rhs))))  -- pattern binding
 *
 * ------------------------------------------------------------------------*/

#define depVal(d) (fst(snd(d)))        /* Access to dependency information */

static List local dependencyAnal(bs)   /* Separate lists of bindings into  */
List bs; {			       /* mutually recursive groups in	   */
				       /* order of dependency		   */

    mapProc(addDepField,bs);	       /* add extra field for dependents   */
    mapProc(depBinding,bs);	       /* find dependents of each binding  */
    bs = bscc(bs);		       /* sort to strongly connected comps */
    mapProc(remDepField,bs);	       /* remove dependency info field	   */
    return bs;
}

static List local topDependAnal(bs)    /* Like dependencyAnal(), but at    */
List bs; {			       /* top level, reporting on progress */
    List xs;
    Int  i = 0;

    setGoal("Dependency analysis",(Target)(length(bs)));
    mapProc(addDepField,bs);	       /* add extra field for dependents   */
    for (xs=bs; nonNull(xs); xs=tl(xs)) {
	depBinding(hd(xs));
	soFar((Target)(i++));
    }
    bs = bscc(bs);		       /* sort to strongly connected comps */
    mapProc(remDepField,bs);	       /* remove dependency info field	   */
    done();
    return bs;
}

static Void local addDepField(b)       /* add extra field to binding to    */
Cell b; {			       /* hold list of dependents	   */
    snd(b) = pair(NIL,snd(b));
}

static Void local remDepField(bs)      /* remove dependency field from	   */
List bs; {			       /* list of bindings		   */
    mapProc(remDepField1,bs);
}

static Void local remDepField1(b)      /* remove dependency field from	   */
Cell b; {			       /* single binding		   */
    snd(b) = snd(snd(b));
}

static Void local clearScope() {       /* initialise dependency scoping    */
    bounds   = NIL;
    bindings = NIL;
    depends  = NIL;
}

static Void local withinScope(bs)      /* enter scope of bindings bs	   */
List bs; {
    bounds   = cons(NIL,bounds);
    bindings = cons(bs,bindings);
    depends  = cons(NIL,depends);
}

static Void local leaveScope() {       /* leave scope of last withinScope  */
    bounds   = tl(bounds);
    bindings = tl(bindings);
    depends  = tl(depends);
}

/* --------------------------------------------------------------------------
 * As a side effect of the dependency analysis we also make the following
 * checks:
 * - Each lhs is a valid pattern/function lhs, all constructor functions
 *   have been defined and are used with the correct number of arguments.
 * - No lhs contains repeated pattern variables.
 * - Expressions used on the rhs of an eqn should be well formed.  This
 *   includes:
 *   - Checking for valid patterns (including repeated vars) in lambda,
 *     case, and list comprehension expressions.
 *   - Recursively checking local lists of equations.
 * - No free (i.e. unbound) variables are used in the declaration list.
 * ------------------------------------------------------------------------*/

static Void local depBinding(b)        /* find dependents of binding	   */
Cell b; {
    Cell defpart = snd(snd(snd(b)));   /* definition part of binding	   */

    hd(depends) = NIL;

    if (isVar(fst(b))) {	       /* function-binding?		   */
	mapProc(depAlt,defpart);
    }
    else {			       /* pattern-binding?		   */
	depRhs(snd(defpart));
    }

    depVal(b) = hd(depends);
}

static Void local depDefaults(c)       /* dependency analysis on defaults  */
Class c; {                             /* from class definition            */
    depClassBindings(cclass(c).defaults);
}

static Void local depInsts(in)         /* dependency analysis on instance  */
Inst in; {                             /* bindings                         */
    depClassBindings(inst(in).implements);
}

static Void local depClassBindings(bs) /* dependency analysis on list of   */
List bs; {                             /* bindings, possibly containing    */
    for (; nonNull(bs); bs=tl(bs))     /* NIL bindings ...                 */
        if (nonNull(hd(bs)))           /* No need to add extra field for   */
	    mapProc(depAlt,name(hd(bs)).defn); /* dependency information.. */
}

static Void local depAlt(a)	       /* find dependents of alternative   */
Cell a; {
    List origBvars = saveBvars();      /* save list of bound variables	   */
    bindPats(rhsLine(snd(a)),fst(a));  /* add new bound vars for patterns  */
    depRhs(snd(a));		       /* find dependents of rhs	   */
    restoreBvars(origBvars);	       /* restore original list of bvars   */
}

static Void local depRhs(r)	       /* find dependents of rhs	   */
Cell r; {
    switch (whatIs(r)) {
	case GUARDED : mapProc(depGuard,snd(r));
		       break;

	case LETREC  : fst(snd(r)) = eqnsToBindings(fst(snd(r)));
		       withinScope(fst(snd(r)));
		       fst(snd(r)) = dependencyAnal(fst(snd(r)));
		       hd(depends) = fst(snd(r));
		       depRhs(snd(snd(r)));
		       leaveScope();
		       break;

	default      : snd(r) = depExpr(intOf(fst(r)),snd(r));
		       break;
    }
}

static Void local depGuard(g)	       /* find dependents of single guarded*/
Cell g; {			       /* expression			   */
    depPair(intOf(fst(g)),snd(g));
}

static Cell local depExpr(line,e)      /* find dependents of expression    */
Int  line;
Cell e; {
    switch (whatIs(e)) {

	case VARIDCELL	:
	case VAROPCELL	: return depVar(line,e);

	case CONIDCELL	:
	case CONOPCELL	: return conDefined(line,textOf(e));

	case AP 	: depPair(line,e);
			  break;

#if BIGNUMS
	case ZERONUM	:
	case POSNUM	:
	case NEGNUM	:
#endif
	case NAME	:
	case TUPLE	:
	case STRCELL	:
	case CHARCELL	:
	case FLOATCELL  :
	case INTCELL	: break;

	case COND	: depTriple(line,snd(e));
			  break;

	case FINLIST	: map1Over(depExpr,line,snd(e));
			  break;

	case LETREC	: fst(snd(e)) = eqnsToBindings(fst(snd(e)));
			  withinScope(fst(snd(e)));
			  fst(snd(e)) = dependencyAnal(fst(snd(e)));
			  hd(depends) = fst(snd(e));
			  snd(snd(e)) = depExpr(line,snd(snd(e)));
			  leaveScope();
			  break;

	case LAMBDA	: depAlt(snd(e));
			  break;

	case DOCOMP	: /* fall-thru */
	case COMP	: depComp(line,snd(e),snd(snd(e)));
			  break;

#if LAZY_ST
	case RUNST	: snd(e) = depExpr(line,snd(e));
			  break;
#endif

	case ESIGN	: fst(snd(e)) = depExpr(line,fst(snd(e)));
			  snd(snd(e)) = checkSigType(line,
						     "expression",
						     fst(snd(e)),
						     snd(snd(e)));
			  break;

	case CASE	: fst(snd(e)) = depExpr(line,fst(snd(e)));
			  map1Proc(depCaseAlt,line,snd(snd(e)));
			  break;

	case CONFLDS	: depConFlds(line,e,FALSE);
			  break;

	case UPDFLDS	: depUpdFlds(line,e);
			  break;

	case ASPAT	: ERRMSG(line) "Illegal `@' in expression"
			  EEND;

	case LAZYPAT	: ERRMSG(line) "Illegal `~' in expression"
			  EEND;

	case WILDCARD	: ERRMSG(line) "Illegal `_' in expression"
			  EEND;

	default 	: internal("in depExpr");
   }
   return e;
}

static Void local depPair(line,e)	/* find dependents of pair of exprs*/
Int  line;
Cell e; {
    fst(e) = depExpr(line,fst(e));
    snd(e) = depExpr(line,snd(e));
}

static Void local depTriple(line,e)	/* find dependents of triple exprs */
Int  line;
Cell e; {
    fst3(e) = depExpr(line,fst3(e));
    snd3(e) = depExpr(line,snd3(e));
    thd3(e) = depExpr(line,thd3(e));
}

static Void local depComp(l,e,qs)	/* find dependents of comprehension*/
Int  l;
Cell e;
List qs; {
    if (isNull(qs))
	fst(e) = depExpr(l,fst(e));
    else {
	Cell q   = hd(qs);
	List qs1 = tl(qs);
	switch (whatIs(q)) {
	    case FROMQUAL : {   List origBvars = saveBvars();
				snd(snd(q))    = depExpr(l,snd(snd(q)));
				fst(snd(q))    = bindPat(l,fst(snd(q)));
				depComp(l,e,qs1);
				restoreBvars(origBvars);
			    }
			    break;

	    case QWHERE   : snd(q)      = eqnsToBindings(snd(q));
			    withinScope(snd(q));
			    snd(q)      = dependencyAnal(snd(q));
			    hd(depends) = snd(q);
			    depComp(l,e,qs1);
			    leaveScope();
			    break;

	    case DOQUAL	  : /* fall-thru */
	    case BOOLQUAL : snd(q) = depExpr(l,snd(q));
			    depComp(l,e,qs1);
			    break;
	}
    }
}

static Void local depCaseAlt(line,a)	/* find dependents of case altern. */
Int  line;
Cell a; {
    List origBvars = saveBvars();	/* save list of bound variables	   */
    fst(a) = bindPat(line,fst(a));	/* add new bound vars for patterns */
    depRhs(snd(a));			/* find dependents of rhs	   */
    restoreBvars(origBvars);		/* restore original list of bvars  */
}

static Cell local depVar(line,e)	/* register occurrence of variable */
Int line;
Cell e; {
    List bounds1   = bounds;
    List bindings1 = bindings;
    List depends1  = depends;
    Text t	   = textOf(e);
    Cell n;

    while (nonNull(bindings1)) {
	n = varIsMember(t,hd(bounds1));   /* look for t in bound variables */
	if (nonNull(n))
	    return n;

	n = findBinding(t,hd(bindings1)); /* look for t in var bindings    */
	if (nonNull(n)) {
	   if (!cellIsMember(n,hd(depends1)))
	       hd(depends1) = cons(n,hd(depends1));
	   return (isVar(fst(n)) ? fst(n) : e);
	}

	bounds1   = tl(bounds1);
	bindings1 = tl(bindings1);
	depends1  = tl(depends1);
    }

    if (isNull(n=findName(t))) {	       /* check global definitions */
	ERRMSG(line) "Undefined variable \"%s\"", textToStr(t)
	EEND;
    }

    return n;
}

static Void local depConFlds(line,e,isP)/* check construction using fields */
Int  line;
Cell e;
Bool isP; {
    Name c = conDefined(line,textOf(fst(snd(e))));
    if (isNull(snd(snd(e))) ||
	nonNull(cellIsMember(c,depFields(line,e,snd(snd(e)),isP))))
	fst(snd(e)) = c;
    else {
	ERRMSG(line) "Constructor \"%s\" does not have selected fields in ",
		     textToStr(name(c).text)
	ETHEN ERREXPR(e);
	ERRTEXT "\n"
	EEND;
    }
    if (!isP && isPair(name(c).defn)) {	/* Check that banged fields defined*/
	List scs = fst(name(c).defn);	/* List of strict components	   */
	Type t   = name(c).type;
	Int  a   = name(c).arity;
	List fs  = snd(snd(e));
	List ss;
	if (isPolyType(t))		/* Find tycon that c belongs to	   */
	    t = monoTypeOf(t);
	if (whatIs(t)==QUAL)
	    t = snd(snd(t));
	while (0<a--)
	    t = arg(t);
	while (isAp(t))
	    t = fun(t);
	for (ss=tycon(t).defn; hasCfun(ss); ss=tl(ss))
	    ;
	/* Now we know the tycon t that c belongs to, and the corresponding
	 * list of selectors for that type, ss.  Now we have to check that
	 * each of the fields identified by scs appears in fs, using ss to
	 * cross reference, and convert integers to selector names.
	 */
	for (; nonNull(scs); scs=tl(scs)) {
	    Int  i   = intOf(hd(scs));
	    List ss1 = ss;
	    for (; nonNull(ss1); ss1=tl(ss1)) {
		List cns = name(hd(ss1)).defn;
		for (; nonNull(cns); cns=tl(cns))
		    if (fst(hd(cns))==c)
			break;
		if (nonNull(cns) && intOf(snd(hd(cns)))==i)
		    break;
	    }
	    if (isNull(ss1))
		internal("depConFlds");
	    else {
		Name s   = hd(ss1);
		List fs1 = fs;
		for (; nonNull(fs1) && s!=fst(hd(fs1)); fs1=tl(fs1))
		    ;
		if (isNull(fs1)) {
		    ERRMSG(line) "Construction does not define strict field"
		    ETHEN
		    ERRTEXT      "\nExpression : " ETHEN ERREXPR(e);
		    ERRTEXT	 "\nField      : " ETHEN ERREXPR(s);
		    ERRTEXT      "\n"
		    EEND;
		}
	    }
	}
    }
}

static Void local depUpdFlds(line,e)	/* check update using fields	   */
Int  line;
Cell e; {
    if (isNull(thd3(snd(e)))) {
	ERRMSG(line) "Empty field list in update"
	EEND;
    }
    fst3(snd(e)) = depExpr(line,fst3(snd(e)));
    snd3(snd(e)) = depFields(line,e,thd3(snd(e)),FALSE);
}

static List local depFields(l,e,fs,isP)	/* check field binding list	   */
Int  l;
Cell e;
List fs;
Bool isP; {
    List cs = NIL;
    List ss = NIL;

    for (; nonNull(fs); fs=tl(fs)) {	/* for each field binding	   */
	Cell fb = hd(fs);
	Name s;

	if (isVar(fb))			/* expand  var  to  var = var	   */
	    fb = hd(fs) = pair(fb,fb);

	s = findName(textOf(fst(fb)));	/* check for selector		   */
	if (nonNull(s) && isSfun(s))
	    fst(fb) = s;
	else {
	    ERRMSG(l) "\"%s\" is not a selector function/field name",
		      textToStr(textOf(fst(fb)))
	    EEND;
	}

	if (isNull(ss)) {		/* for first named selector	   */
	    List scs = name(s).defn;	/* calculate list of constructors  */
	    for (; nonNull(scs); scs=tl(scs))
		cs = cons(fst(hd(scs)),cs);
	    ss = singleton(s);		/* initialize selector list	   */
	}
	else {				/* for subsequent selectors	   */
	    List ds = cs;		/* intersect constructor lists	   */
	    for (cs=NIL; nonNull(ds); ) {
		List scs = name(s).defn;
		while (nonNull(scs) && fst(hd(scs))!=hd(ds))
		    scs = tl(scs);
		if (isNull(scs))
		    ds = tl(ds);
		else {
		    List next = tl(ds);
		    tl(ds)    = cs;
		    cs	      = ds;
		    ds        = next;
		}
	    }

	    if (cellIsMember(s,ss)) {	/* check for repeated uses	   */
		ERRMSG(l) "Repeated field name \"%s\" in field list",
			  textToStr(name(s).text)
		EEND;
	    }
	    ss = cons(s,ss);
	}

	if (isNull(cs)) {		/* Are there any matching constrs? */
	    ERRMSG(l) "No constructor has all of the fields specified in "
	    ETHEN ERREXPR(e);
	    ERRTEXT "\n"
	    EEND;
	}

	snd(fb) = (isP ? checkPat(l,snd(fb)) : depExpr(l,snd(fb)));
    }
    return cs;
}

/* --------------------------------------------------------------------------
 * Several parts of this program require an algorithm for sorting a list
 * of values (with some added dependency information) into a list of strongly
 * connected components in which each value appears before its dependents.
 *
 * Each of these algorithms is obtained by parameterising a standard
 * algorithm in "scc.c" as shown below.
 * ------------------------------------------------------------------------*/

#define visited(d) (isInt(DEPENDS(d)))	/* binding already visited ?	   */

static Cell daSccs = NIL;
static Int  daCount;

static Int local sccMin(x,y)	       /* calculate minimum of x,y (unless */
Int x,y; {			       /* y is zero)			   */
    return (x<=y || y==0) ? x : y;
}

#define  SCC2		 tcscc		/* make scc algorithm for Tycons   */
#define  LOWLINK	 tclowlink
#define  DEPENDS(c)      (isTycon(c) ? tycon(c).kind : cclass(c).sig)
#define  SETDEPENDS(c,v) if(isTycon(c)) tycon(c).kind=v; else cclass(c).sig=v
#include "scc.c"
#undef   SETDEPENDS
#undef	 DEPENDS
#undef 	 LOWLINK
#undef	 SCC2

#define  SCC		 bscc		/* make scc algorithm for Bindings */
#define  LOWLINK	 blowlink
#define  DEPENDS(t)	 depVal(t)
#define  SETDEPENDS(c,v) depVal(c)=v
#include "scc.c"
#undef   SETDEPENDS
#undef	 DEPENDS
#undef 	 LOWLINK
#undef	 SCC

/* --------------------------------------------------------------------------
 * Main static analysis:
 * ------------------------------------------------------------------------*/

Void checkExp() {			/* Top level static check on Expr  */
    staticAnalysis(RESET);
    clearScope();			/* Analyse expression in the scope */
    withinScope(NIL);			/* of no local bindings		   */
    inputExpr = depExpr(0,inputExpr);
    leaveScope();
    staticAnalysis(RESET);
}

Void checkDefns() {			/* Top level static analysis	   */
    staticAnalysis(RESET);

    linkPreludeTC();			/* Get prelude tycons and classes  */
    mapProc(checkTyconDefn,tyconDefns);	/* validate tycon definitions	   */
    checkSynonyms(tyconDefns);		/* check synonym definitions	   */
    mapProc(checkClassDefn,classDefns);	/* process class definitions	   */
    mapProc(kindTCGroup,tcscc(tyconDefns,classDefns)); /* attach kinds	   */
    deriveEval(tyconDefns);		/* derive instances of Eval	   */
    tyconDefns = NIL;
    mapProc(addMembers,classDefns);	/* add definitions for member funs */
    mapProc(visitClass,classDefns);	/* check class hierarchy	   */

    mapProc(checkPrimDefn,primDefns);	/* check primitive declarations	   */
    primDefns = NIL;

    instDefns = rev(instDefns);		/* process instance definitions	   */
    mapProc(checkInstDefn,instDefns);

    linkPreludeCM();			/* Get prelude cfuns and mfuns	   */
    deriveContexts(derivedInsts);	/* check derived instances	   */
    instDefns = appendOnto(instDefns,derivedInsts);
    mapProc(checkInstSC,instDefns);
    checkDefaultDefns();		/* validate default definitions	   */

    mapProc(addRSsigdecls,typeInDefns);	/* add sigdecls for RESTRICTSYN	   */
    valDefns = eqnsToBindings(valDefns);/* translate value equations	   */
    map1Proc(opDefined,valDefns,opDefns);/*check all declared ops bound	   */
    mapProc(allNoPrevDef,valDefns);	/* check against previous defns	   */

    mapProc(checkTypeIn,typeInDefns);	/* check restricted synonym defns  */

    clearScope();
    withinScope(valDefns);
    valDefns = topDependAnal(valDefns); /* top level dependency ordering   */
    mapProc(depDefaults,classDefns);    /* dep. analysis on class defaults */
    mapProc(depInsts,instDefns);        /* dep. analysis on inst defns	   */
    leaveScope();

    evalDefaults = defaultDefns;	/* Set defaults for evaluator	   */

    staticAnalysis(RESET);
}

static Void local addRSsigdecls(pr)	/* add sigdecls from TYPE ... IN ..*/
Pair pr; {
    List vs = snd(pr);			/* get list of variables	   */
    for (; nonNull(vs); vs=tl(vs)) {
	if (fst(hd(vs))==SIGDECL) {	/* find a sigdecl		   */
	    valDefns = cons(hd(vs),valDefns);	/* add to valDefns	   */
	    hd(vs)   = hd(snd3(snd(hd(vs))));	/* and replace with var	   */
	}
    }
}

static Void local opDefined(bs,op)	 /* check that op bound in bs	   */
List bs;				 /* (or in current module for	   */
Cell op; {				 /* constructor functions etc...)  */
    Name n;

    if (isNull(findBinding(textOf(op),bs))
           && (isNull(n=findName(textOf(op))) || !nameThisModule(n))) {
	ERRMSG(0) "No top level definition for operator symbol \"%s\"",
		  textToStr(textOf(op))
	EEND;
    }
}

static Void local allNoPrevDef(b)	 /* ensure no previous bindings for*/
Cell b; {				 /* variables in new binding	   */
    if (isVar(fst(b)))
	noPrevDef(rhsLine(snd(hd(snd(snd(b))))),fst(b));
    else {
	Int line = rhsLine(snd(snd(snd(b))));
	map1Proc(noPrevDef,line,fst(b));
    }
}

static Void local noPrevDef(line,v)	 /* ensure no previous binding for */
Int  line;				 /* new variable		   */
Cell v; {
    Name n = findName(textOf(v));

    if (isNull(n)) {
	n            = newName(textOf(v));
	name(n).defn = PREDEFINED;
    }
    else if (name(n).defn!=PREDEFINED) {
	ERRMSG(line) "Attempt to redefine variable \"%s\"",
		     textToStr(name(n).text)
	EEND;
    }
    name(n).line = line;
}

static Void local checkTypeIn(cvs)	/* Check that vars in restricted   */
Pair cvs; {				/* synonym are defined, and replace*/
    Tycon c  = fst(cvs);		/* vars with names		   */
    List  vs = snd(cvs);

    for (; nonNull(vs); vs=tl(vs))
	if (isNull(findName(textOf(hd(vs))))) {
	    ERRMSG(tycon(c).line)
		"No top level binding of \"%s\" for restricted synonym \"%s\"",
		textToStr(textOf(hd(vs))), textToStr(tycon(c).text)
	    EEND;
	}
}

/* --------------------------------------------------------------------------
 * Static Analysis control:
 * ------------------------------------------------------------------------*/

Void staticAnalysis(what)
Int what; {
    switch (what) {
	case INSTALL :
	case RESET   : cfunSfuns    = NIL;
		       daSccs	    = NIL;
		       patVars	    = NIL;
		       bounds	    = NIL;
		       bindings	    = NIL;
		       depends      = NIL;
		       tcDeps	    = NIL;
		       derivedInsts = NIL;
		       diVars	    = NIL;
		       diNum	    = 0;
		       break;

	case MARK    : mark(daSccs);
		       mark(patVars);
		       mark(bounds);
		       mark(bindings);
		       mark(depends);
		       mark(tcDeps);
		       mark(derivedInsts);
		       mark(diVars);
		       mark(cfunSfuns);
		       break;
    }
}

/*-------------------------------------------------------------------------*/
