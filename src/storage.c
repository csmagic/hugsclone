/* --------------------------------------------------------------------------
 * storage.c:   Copyright (c) Mark P Jones 1991-1996.   All rights reserved.
 *              See NOTICE for details and conditions of use etc...
 *              Hugs version 1.3, August 1996
 *
 * Primitives for manipulating global data structures
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include <setjmp.h>

/*#define DEBUG_SHOWUSE*/

static Int  local hash                  Args((String));
static Int  local saveText              Args((Text));
static List local insertTycon           Args((Tycon,List));
static List local insertName            Args((Name,List));
static Void local patternError          Args((String));
static Bool local stringMatch           Args((String,String));
static Void local killDicts		Args((Int,Int));
static Cell local markCell              Args((Cell));
static Void local markSnd               Args((Cell));
static Cell local indirectChain         Args((Cell));
static Void local moveFlat		Args((Cell));
static Cell local lowLevelLastIn        Args((Cell));
static Cell local lowLevelLastOut       Args((Cell));
#if IO_MONAD
static Void local freeHandle            Args((Int));
#endif

/* --------------------------------------------------------------------------
 * Text storage:
 *
 * provides storage for the characters making up identifier and symbol
 * names, string literals, character constants etc...
 *
 * All character strings are stored in a large character array, with textHw
 * pointing to the next free position.  Lookup in the array is improved using
 * a hash table.  Internally, text strings are represented by integer offsets
 * from the beginning of the array to the string in question.
 *
 * Where memory permits, the use of multiple hashtables gives a significant
 * increase in performance, particularly when large source files are used.
 *
 * Each string in the array is terminated by a zero byte.  No string is
 * stored more than once, so that it is safe to test equality of strings by
 * comparing the corresponding offsets.
 *
 * Special text values (beyond the range of the text array table) are used
 * to generate unique `new variable names' as required.
 *
 * The same text storage is also used to hold text values stored in a saved
 * expression.  This grows downwards from the top of the text table (and is
 * not included in the hash table).
 * ------------------------------------------------------------------------*/

#define TEXTHSZ 512                     /* Size of Text hash table         */
#define NOTEXT  ((Text)(~0))            /* Empty bucket in Text hash table */
static  Text    textHw;                 /* Next unused position            */
static  Text    savedText = NUM_TEXT;   /* Start of saved portion of text  */
static  Text    nextNewText;            /* Next new text value             */
static  Text    nextNewDText;           /* Next new dict text value        */
static  char    DEFTABLE(text,NUM_TEXT);/* Storage of character strings    */
static  Text    textHash[TEXTHSZ][NUM_TEXTH]; /* Hash table storage        */

String textToStr(t)                    /* find string corresp to given Text*/
Text t; {
    static char newVar[16];

    if (0<=t && t<NUM_TEXT)                     /* standard char string    */
	return text + t;
    if (t<0)
	sprintf(newVar,"d%d",-t);               /* dictionary variable     */
    else
	sprintf(newVar,"v%d",t-NUM_TEXT);       /* normal variable         */
    return newVar;
}

Text inventText() {                     /* return new unused variable name */
    return nextNewText++;
}

Text inventDictText() {                 /* return new unused dictvar name  */
    return nextNewDText--;
}

Bool inventedText(t)                    /* Signal TRUE if text has been    */
Text t; {                               /* generated internally            */
    return (t<0 || t>=NUM_TEXT);
}

static Int local hash(s)                /* Simple hash function on strings */
String s; {
    int v, j = 3;

    for (v=((int)(*s))*8; *s; s++)
	v += ((int)(*s))*(j++);
    if (v<0)
	v = (-v);
    return(v%TEXTHSZ);
}

Text findText(s)                       /* Locate string in Text array      */
String s; {
    int    h       = hash(s);
    int    hashno  = 0;
    Text   textPos = textHash[h][hashno];

#define TryMatch        {   Text   originalTextPos = textPos;              \
			    String t;                                      \
			    for (t=s; *t==text[textPos]; textPos++,t++)    \
				if (*t=='\0')                              \
				    return originalTextPos;                \
			}
#define Skip            while (text[textPos++]) ;

    while (textPos!=NOTEXT) {
	TryMatch
	if (++hashno<NUM_TEXTH)         /* look in next hashtable entry    */
	    textPos = textHash[h][hashno];
	else {
	    Skip
	    while (textPos < textHw) {
		TryMatch
		Skip
	    }
	    break;
	}
    }

#undef TryMatch
#undef Skip

    textPos = textHw;                  /* if not found, save in array      */
    if (textHw + strlen(s) + 1 > savedText) {
	ERRMSG(0) "Character string storage space exhausted"
	EEND;
    }
    while (text[textHw++] = *s++)
	;
    if (hashno<NUM_TEXTH) {            /* updating hash table as necessary */
	textHash[h][hashno] = textPos;
	if (hashno<NUM_TEXTH-1)
	    textHash[h][hashno+1] = NOTEXT;
    }

    return textPos;
}

static Int local saveText(t)            /* Save text value in buffer       */
Text t; {                               /* at top of text table            */
    String s = textToStr(t);
    Int    l = strlen(s);

    if (textHw + strlen(s) + 1 > savedText) {
	ERRMSG(0) "Character string storage space exhausted"
	EEND;
    }
    savedText -= l+1;
    strcpy(text+savedText,s);
    return savedText;
}

/* --------------------------------------------------------------------------
 * Syntax storage:
 *
 * Operator declarations are stored in a table which associates Text values
 * with Syntax values.
 * ------------------------------------------------------------------------*/

static Int syntaxHw;                   /* next unused syntax table entry   */
static struct strSyntax {              /* table of Text <-> Syntax values  */
    Text   text;
    Syntax syntax;
} DEFTABLE(tabSyntax,NUM_SYNTAX);

Syntax syntaxOf(t)                     /* look up syntax of operator symbol*/
Text t; {
    int i;

    for (i=0; i<syntaxHw; ++i)
	if (tabSyntax[i].text==t)
	    return tabSyntax[i].syntax;
    return defaultSyntax(t);
}

Void addSyntax(line,t,sy)              /* add (t,sy) to syntax table       */
Int    line;
Text   t;
Syntax sy; {
    int i;

    for (i=0; i<syntaxHw; ++i)
	if (tabSyntax[i].text==t) {
	    ERRMSG(line) "Attempt to redefine syntax of operator \"%s\"",
			 textToStr(t)
	    EEND;
	}

    if (syntaxHw>=NUM_SYNTAX) {
	ERRMSG(line) "Too many fixity declarations"
	EEND;
    }

    tabSyntax[syntaxHw].text   = t;
    tabSyntax[syntaxHw].syntax = sy;
    syntaxHw++;
}

/* --------------------------------------------------------------------------
 * Addr storage: records `next unused program location'
 * ------------------------------------------------------------------------*/

static Addr addrHw;                    /* next unused program location     */

Addr getMem(n)                         /* Get some more memory             */
Int n; {
    Addr newAddr = addrHw;
    addrHw += n;
    if (addrHw>=NUM_ADDRS) {
	ERRMSG(0) "Program code storage space exhausted"
	EEND;
    }
    return newAddr;
}

Void nextInstr(a)                       /* Reset point to next instruction */
Addr a; {                               /* Currently does NO CHECKING      */
    addrHw = a;
}

/* --------------------------------------------------------------------------
 * Tycon storage:
 *
 * A Tycon represents a user defined type constructor.  Tycons are indexed
 * by Text values ... a very simple hash function is used to improve lookup
 * times.  Tycon entries with the same hash code are chained together, with
 * the most recent entry at the front of the list.
 * ------------------------------------------------------------------------*/

#define TYCONHSZ 256                            /* Size of Tycon hash table*/
#define tHash(x) ((x)%TYCONHSZ)                 /* Tycon hash function     */
static  Tycon    tyconHw;                       /* next unused Tycon       */
static  Tycon    DEFTABLE(tyconHash,TYCONHSZ);  /* Hash table storage      */
struct  strTycon DEFTABLE(tabTycon,NUM_TYCON);  /* Tycon storage           */

Tycon newTycon(t)                       /* add new tycon to tycon table    */
Text t; {
    Int h = tHash(t);

    if (tyconHw-TYCMIN >= NUM_TYCON) {
	ERRMSG(0) "Type constructor storage space exhausted"
	EEND;
    }
    tycon(tyconHw).text          = t;   /* clear new tycon record          */
    tycon(tyconHw).kind          = NIL;
    tycon(tyconHw).defn          = NIL;
    tycon(tyconHw).what          = NIL;
    tycon(tyconHw).nextTyconHash = tyconHash[h];
    tyconHash[h]                 = tyconHw;

    return tyconHw++;
}

Tycon findTycon(t)                      /* locate Tycon in tycon table     */
Text t; {
    Tycon tc = tyconHash[tHash(t)];

    while (nonNull(tc) && tycon(tc).text!=t)
	tc = tycon(tc).nextTyconHash;
    return tc;
}

Tycon addPrimTycon(s,kind,ar,what,defn) /* add new primitive type constr   */
String s;
Kind   kind;
Int    ar;
Cell   what;
Cell   defn; {
    Tycon tc        = newTycon(findText(s));
    tycon(tc).line  = 0;
    tycon(tc).kind  = kind;
    tycon(tc).what  = what;
    tycon(tc).defn  = defn;
    tycon(tc).arity = ar;
    return tc;
}

static List local insertTycon(tc,ts)    /* insert tycon tc into sorted list*/
Tycon tc;                               /* ts                              */
List  ts; {
    Cell   prev = NIL;
    Cell   curr = ts;
    String s    = textToStr(tycon(tc).text);

    while (nonNull(curr) && strCompare(s,textToStr(tycon(hd(curr)).text))>=0) {
	if (hd(curr)==tc)               /* just in case we get duplicates! */
	    return ts;
	prev = curr;
	curr = tl(curr);
    }
    if (nonNull(prev)) {
	tl(prev) = cons(tc,curr);
	return ts;
    }
    else
	return cons(tc,curr);
}

List addTyconsMatching(pat,ts)          /* Add tycons matching pattern pat */
String pat;                             /* to list of Tycons ts            */
List   ts; {                            /* Null pattern matches every tycon*/
    Tycon tc;
    for (tc=TYCMIN; tc<tyconHw; ++tc)
	if (!pat || stringMatch(pat,textToStr(tycon(tc).text)))
	    ts = insertTycon(tc,ts);
    return ts;
}

/* --------------------------------------------------------------------------
 * Name storage:
 *
 * A Name represents a top level binding of a value to an identifier.
 * Such values may be a constructor function, a member function in a
 * class, a user-defined or primitive value/function.
 *
 * Names are indexed by Text values ... a very simple hash functions speeds
 * access to the table of Names and Name entries with the same hash value
 * are chained together, with the most recent entry at the front of the
 * list.
 * ------------------------------------------------------------------------*/

#define NAMEHSZ  256                            /* Size of Name hash table */
#define nHash(x) ((x)%NAMEHSZ)                  /* hash fn :: Text->Int    */
static  Name     nameHw;                        /* next unused name        */
static  Name     DEFTABLE(nameHash,NAMEHSZ);    /* Hash table storage      */
struct  strName  DEFTABLE(tabName,NUM_NAME);    /* Name table storage      */

Name newName(t)                         /* add new name to name table      */
Text t; {
    Int h = nHash(t);

    if (nameHw-NAMEMIN >= NUM_NAME) {
	ERRMSG(0) "Name storage space exhausted"
	EEND;
    }
    name(nameHw).text         = t;      /* clear new name record           */
    name(nameHw).line         = 0;
    name(nameHw).arity        = 0;
    name(nameHw).number       = EXECNAME;
    name(nameHw).defn         = NIL;
    name(nameHw).type         = NIL;
    name(nameHw).primDef      = 0;
    name(nameHw).code         = 0;
    name(nameHw).nextNameHash = nameHash[h];
    nameHash[h]               = nameHw;

    return nameHw++;
}

Name findName(t)                        /* locate name in name table       */
Text t; {
    Name n = nameHash[nHash(t)];

    while (nonNull(n) && name(n).text!=t)
	n = name(n).nextNameHash;
    return n;
}

Void addPrim(l,n,s,ty)                  /* add primitive function value    */
Int    l;
Name   n;
String s;
Type   ty; {
    Int  i;

    name(n).line = l;
    name(n).defn = NIL;
    name(n).type = ty;

    for (i=0; primitives[i].ref; ++i)
	if (strcmp(s,primitives[i].ref)==0) {
	    name(n).arity   = primitives[i].arity;
	    name(n).primDef = primitives[i].imp;
	    return;
	}
    externalPrim(n,s);
}

Name addPrimCfun(t,arity,no,type)       /* add primitive constructor func. */
Text t;
Int  arity;
Int  no;
Cell type; {
    Name n          = newName(t);
    name(n).arity   = arity;
    name(n).number  = cfunNo(no);
    name(n).type    = type;
    name(n).primDef = 0;
    return n;
}

Int sfunPos(s,c)			/* Find position of field with	   */
Name s;					/* selector s in constructor c.	   */
Name c; {
    List cns;
    cns = name(s).defn;
    for (; nonNull(cns); cns=tl(cns))
	if (fst(hd(cns))==c)
	    return intOf(snd(hd(cns)));
    internal("sfunPos");
    return 0;/*NOTREACHED*/
}

static List local insertName(nm,ns)     /* insert name nm into sorted list */
Name nm;                                /* ns                              */
List ns; {
    Cell   prev = NIL;
    Cell   curr = ns;
    String s    = textToStr(name(nm).text);

    while (nonNull(curr) && strCompare(s,textToStr(name(hd(curr)).text))>=0) {
	if (hd(curr)==nm)               /* just in case we get duplicates! */
	    return ns;
	prev = curr;
	curr = tl(curr);
    }
    if (nonNull(prev)) {
	tl(prev) = cons(nm,curr);
	return ns;
    }
    else
	return cons(nm,curr);
}

List addNamesMatching(pat,ns)           /* Add names matching pattern pat  */
String pat;                             /* to list of names ns             */
List   ns; {                            /* Null pattern matches every name */
    Name nm;
    for (nm=NAMEMIN; nm<nameHw; ++nm)
	if (!inventedText(name(nm).text)) {
	    String str = textToStr(name(nm).text);
	    if (str[0]!='_' && (!pat || stringMatch(pat,str)))
		ns = insertName(nm,ns);
	}
    return ns;
}

/* --------------------------------------------------------------------------
 * A simple string matching routine
 *     `*'    matches any sequence of zero or more characters
 *     `?'    matches any single character exactly 
 *     `@str' matches the string str exactly (ignoring any special chars)
 *     `\c'   matches the character c only (ignoring special chars)
 *     c      matches the character c only
 * ------------------------------------------------------------------------*/

static Void local patternError(s)       /* report error in pattern         */
String s; {
    ERRMSG(0) "%s in pattern", s
    EEND;
}

static Bool local stringMatch(pat,str)  /* match string against pattern    */
String pat;
String str; {

    for (;;)
	switch (*pat) {
	    case '\0' : return (*str=='\0');

	    case '*'  : do {
			    if (stringMatch(pat+1,str))
				return TRUE;
			} while (*str++);
			return FALSE;

	    case '?'  : if (*str++=='\0')
			    return FALSE;
			pat++;
			break;

	    case '['  : {   Bool found = FALSE;
			    while (*++pat!='\0' && *pat!=']')
				if (!found && ( pat[0] == *str  ||
					       (pat[1] == '-'   &&
						pat[2] != ']'   &&
						pat[2] != '\0'  &&
						pat[0] <= *str  &&
						pat[2] >= *str)))

				    found = TRUE;
			    if (*pat != ']')
				patternError("missing `]'");
			    if (!found)
				return FALSE;
			    pat++;
			    str++;
			}
			break;

	    case '\\' : if (*++pat == '\0')
			    patternError("extra trailing `\\'");
			/*fallthru!*/
	    default   : if (*pat++ != *str++)
			    return FALSE;
			break;
	}
}

/* --------------------------------------------------------------------------
 * Storage of type classes, instances etc...:
 * ------------------------------------------------------------------------*/

static Class classHw;                  /* next unused class                */
static Inst  instHw;                   /* next unused instance record      */

struct strClass DEFTABLE(tabClass,NUM_CLASSES); /* table of class records  */
struct strInst far *tabInst;           /* (pointer to) table of instances  */

Class newClass(t)                      /* add new class to class table     */
Text t; {
    if (classHw-CLASSMIN >= NUM_CLASSES) {
	ERRMSG(0) "Class storage space exhausted"
	EEND;
    }
    cclass(classHw).text      = t;
    cclass(classHw).sig       = NIL;
    cclass(classHw).supers    = NIL;
    cclass(classHw).members   = NIL;
    cclass(classHw).defaults  = NIL;
    cclass(classHw).instances = NIL;
    return classHw++;
}

Class classMax() {                      /* Return max Class in use ...     */
    return classHw;                     /* This is a bit ugly, but it's not*/
}                                       /* worth a lot of effort right now */

Class findClass(t)                     /* look for named class in table    */
Text t; {
    Class c;

    for (c=CLASSMIN; c<classHw; c++)
	if (cclass(c).text==t)
	    return c;
    return NIL;
}

Inst newInst() {                       /* add new instance to table        */
    if (instHw-INSTMIN >= NUM_INSTS) {
	ERRMSG(0) "Instance storage space exhausted"
	EEND;
    }
    inst(instHw).specifics  = NIL;
    inst(instHw).implements = NIL;
    inst(instHw).dicts      = NIL;
    inst(instHw).superBuild = NIL;

    return instHw++;
}

Inst findInst(c,t)                      /* find instance for specific C-T  */
Class c;                                /* where t is a Tycon (not synonym)*/
Type  t; {                              /* or tuple                        */
    List is = cclass(c).instances;
    for (; nonNull(is); is=tl(is))
	if (inst(hd(is)).t == t)
	    return hd(is);
    return NIL;
}

Inst findFirstInst(tc)                  /* look for 1st instance involving */
Tycon tc; {                             /* the type constructor tc         */
    Class c;
    for (c=CLASSMIN; c<classHw; c++) {
	Inst in = findInst(c,tc);
	if (nonNull(in))
	    return in;
    }
    return NIL;
}

Inst findNextInst(tc,in)                /* look for next instance involving*/
Tycon tc;                               /* the type constructor tc         */
Inst  in; {                             /* starting after instance in      */
    Class c = inst(in).c + 1;
    for (; c<classHw; c++) 
	if (nonNull(in=findInst(c,tc)))
	    return in;
    return NIL;
}

Cell makeInstPred(in)                   /* build predicate for instance in */
Inst in; {
    Cell r = inst(in).t;
    Int  i = 0;
    for (; i<inst(in).arity; ++i)
	r = ap(r,mkOffset(i));
    return ap(inst(in).c,r);
}

/* --------------------------------------------------------------------------
 * Control stack:
 *
 * Various parts of the system use a stack of cells.  Most of the stack
 * operations are defined as macros, expanded inline.
 * ------------------------------------------------------------------------*/

Cell     DEFTABLE(cellStack,NUM_STACK);/* Storage for cells on stack       */
#ifndef  GLOBALsp
StackPtr sp;                           /* stack pointer                    */
#endif

Void stackOverflow() {                 /* Report stack overflow            */
    ERRMSG(0) "Control stack overflow"
    EEND;
}

/* --------------------------------------------------------------------------
 * Module storage:
 *
 * script files are read into the system one after another.  The state of
 * the stored data structures (except the garbage-collected heap) is recorded
 * before reading a new script.  In the event of being unable to read the
 * script, or if otherwise requested, the system can be restored to its
 * original state immediately before the file was read.
 * ------------------------------------------------------------------------*/

typedef struct {                       /* record of storage state prior to */
    Text  textHw;                      /* reading script/module            */
    Text  nextNewText;
    Text  nextNewDText;
    Int   syntaxHw;
    Addr  addrHw;
    Tycon tyconHw;
    Name  nameHw;
    Class classHw;
    Inst  instHw;
} module;

#ifdef  DEBUG_SHOWUSE
static Void local showUse(msg,val,mx)
String msg;
Int val, mx; {
    printf("%6s : %d of %d (%d%%)\n",msg,val,mx,(100*val)/mx);
}
#endif

static Module moduleHw;                     /* next unused module number   */
static module DEFTABLE(modules,NUM_SCRIPTS);/* storage for module records  */

Module startNewModule() {              /* start new module, keeping record */
    if (moduleHw >= NUM_SCRIPTS) {     /* of status for later restoration  */
	ERRMSG(0) "Too many script/module files in use"
	EEND;
    }
#ifdef DEBUG_SHOWUSE
    showUse("Text",   textHw,           NUM_TEXT);
    showUse("Syntax", syntaxHw,         NUM_SYNTAX);
    showUse("Addr",   addrHw,           NUM_ADDRS);
    showUse("Tycon",  tyconHw-TYCMIN,   NUM_TYCON);
    showUse("Name",   nameHw-NAMEMIN,   NUM_NAME);
    showUse("Class",  classHw-CLASSMIN, NUM_CLASSES);
    showUse("Inst",   instHw-INSTMIN,   NUM_INSTS);
#endif

    modules[moduleHw].textHw       = textHw;
    modules[moduleHw].nextNewText  = nextNewText;
    modules[moduleHw].nextNewDText = nextNewDText;
    modules[moduleHw].syntaxHw     = syntaxHw;
    modules[moduleHw].addrHw       = addrHw;
    modules[moduleHw].tyconHw      = tyconHw;
    modules[moduleHw].nameHw       = nameHw;
    modules[moduleHw].classHw      = classHw;
    modules[moduleHw].instHw       = instHw;
    return moduleHw++;
}

Bool nameThisModule(n)                  /* Test if given name is defined in*/
Name n; {                               /* current module                  */
    return moduleHw<1 || n>=modules[moduleHw-1].nameHw;
}

#define moduleThis(nm,t,tag)            Module nm(x)                       \
					t x; {                             \
					    Module m=0;                    \
					    while (m<moduleHw              \
						   && x>=modules[m].tag)   \
						m++;                       \
					    return m;                      \
					}
moduleThis(moduleThisName,Name,nameHw)
moduleThis(moduleThisTycon,Tycon,tyconHw)
moduleThis(moduleThisInst,Inst,instHw)
moduleThis(moduleThisClass,Class,classHw)
#undef moduleThis

Void dropModulesFrom(mno)               /* Restore storage to state prior  */
Module mno; {                           /* to reading module mno           */
    if (mno<moduleHw) {                 /* is there anything to restore?   */
	int i;
	killDicts(instHw,modules[mno].instHw);
	textHw       = modules[mno].textHw;
	nextNewText  = modules[mno].nextNewText;
	nextNewDText = modules[mno].nextNewDText;
	syntaxHw     = modules[mno].syntaxHw;
	addrHw       = modules[mno].addrHw;
	tyconHw      = modules[mno].tyconHw;
	nameHw       = modules[mno].nameHw;
	classHw      = modules[mno].classHw;
	instHw       = modules[mno].instHw;

	for (i=0; i<TEXTHSZ; ++i) {
	    int j = 0;
	    while (j<NUM_TEXTH && textHash[i][j]!=NOTEXT
			       && textHash[i][j]<textHw)
		++j;
	    if (j<NUM_TEXTH)
		textHash[i][j] = NOTEXT;
	}

	for (i=0; i<TYCONHSZ; ++i) {
	    Tycon tc = tyconHash[i];
	    while (nonNull(tc) && tc>=tyconHw)
		tc = tycon(tc).nextTyconHash;
	    tyconHash[i] = tc;
	}

	for (i=0; i<NAMEHSZ; ++i) {
	    Name n = nameHash[i];
	    while (nonNull(n) && n>=nameHw)
		n = name(n).nextNameHash;
	    nameHash[i] = n;
	}

	for (i=CLASSMIN; i<classHw; i++) {
	    List in = cclass(i).instances;
	    List is = NIL;

	    while (nonNull(in)) {
		List temp = tl(in);
		if (hd(in)<instHw) {
		    tl(in) = is;
		    is     = in;
		}
		in = temp;
	    }
	    cclass(i).instances = rev(is);
	}

	moduleHw = mno;
    }
}

static Void local killDicts(oldi,newi)	/* Kill dead dictionaries, that is,*/
Int oldi;				/* dictionaries built from insts   */
Int newi; {				/* between newi < oldi.		   */
    List dead = NIL;			/* Build a list of all dead dicts  */
    Int  i;
    for (i=newi; i<oldi; ++i)
	dead = revOnto(inst(i).dicts,dead);

    while (nonNull(dead)) {		/* If there are any dead dicts,    */
	List gen = dead;		/* then we may need to kill off	   */
	dead     = NIL;			/* some live dicts that depend on  */
	for (i=INSTMIN; i<newi; ++i) {	/* dead components		   */
	    List ds = inst(i).dicts;
	    Int  st = dictSupersStart(inst(i).c);
	    List nx = NIL;
	    while (nonNull(ds)) {	/* Scan list of dicts for inst i   */
		Cell d = hd(ds);
		Int  j = dictLength(i);
		for (; j>=st; j--)	/* Look for dead components	   */
		    if (cellIsMember(dictGet(d,j),gen))
			break;
		if (j>=st)		/* Found one, so add to next gen.  */
		    if (nonNull(nx)) {
			tl(nx) = tl(ds);
			tl(ds) = dead;
			dead   = ds;
			ds     = tl(nx);
		    }
		    else {
			inst(i).dicts = tl(ds);
			tl(ds)	      = dead;
			dead	      = ds;
			ds	      = inst(i).dicts;
		    }
		else {			/* Dictionary survives!		   */
		    nx = ds;
		    ds = tl(nx);
		}
	    }
	}
	for (; nonNull(gen); gen=tl(gen))
	    fst(hd(gen)) = INTCELL;	/* Truly, this kills dictionaries  */
    }
}

/* --------------------------------------------------------------------------
 * Heap storage:
 *
 * Provides a garbage collectable heap for storage of expressions etc.
 *
 * Now incorporates a flat resource:  A two-space collected extension of
 * the heap that provides storage for contiguous arrays of Cell storage,
 * cooperating with the garbage collection mechanisms for the main heap.
 * ------------------------------------------------------------------------*/

Int     heapSize = DEFAULTHEAP;         /* number of cells in heap         */
Heap    heapFst;                        /* array of fst component of pairs */
Heap    heapSnd;                        /* array of snd component of pairs */
#ifndef GLOBALfst
Heap    heapTopFst;
#endif
#ifndef GLOBALsnd
Heap    heapTopSnd;
#endif
Bool    consGC = TRUE;                  /* Set to FALSE to turn off gc from*/
					/* C stack; use with extreme care! */
#if     PROFILING
Heap    heapThd, heapTopThd;            /* to keep record of producers     */
Int     sysCount;                       /* record unattached cells         */
Name    producer;                       /* current producer, if any        */
Int     profInterval;                   /* record interval between samples */
FILE    *profile = 0;                   /* pointer to profiler log, if any */
#endif
Long    numCells;
Int     numberGcs;                      /* number of garbage collections   */
Int     cellsRecovered;                 /* number of cells recovered       */

static  Cell freeList;                  /* free list of unused cells       */
static  Cell lsave, rsave;              /* save components of pair         */

static Int  maxFlat = NUM_FLAT;         /* size of flat resource half-space */
Heap   flatspace;                       /* start of active half-space       */
static Int  flatpos;                    /* alloc position with flatspace    */
static Heap tospace;                    /* to-space start and position, used*/
static Int  topos;                      /* during garbage collection.       */

Cell pair(l,r)                          /* Allocate pair (l, r) from       */
Cell l, r; {                            /* heap, garbage collecting first  */
    Cell c = freeList;                  /* if necessary ...                */

    if (isNull(c)) {
	lsave = l;
	rsave = r;
	garbageCollect();
	l     = lsave;
	lsave = NIL;
	r     = rsave;
	rsave = NIL;
	c     = freeList;
    }
    freeList = snd(freeList);
    fst(c)   = l;
    snd(c)   = r;
#if PROFILING
    thd(c)   = producer;
#endif
    numCells++;
    return c;
}

Cell flatAlloc(tag,len)			/* Allocate space in flat resource */
Int tag;				/* with corresponding heap pointer */
Int len; {
    Cell fpr = NIL;

    if (flatpos+len+2 >= maxFlat || isNull(freeList)) {
	garbageCollect();
	if (flatpos+len+2 >= maxFlat) {
	    ERRMSG(0) "Flat resource space exhausted"
	    EEND;
	}
    }

    fpr                  = freeList;	/* Allocate flatcell in main heap  */
    freeList             = tl(freeList);
    fst(fpr)             = tag;		/* In current heap, pointing at    */
    snd(fpr)             = flatpos;	/* current position in flatspace   */
    flatspace[flatpos++] = len;		/* Save length,			   */
    flatspace[flatpos++] = fpr;		/* main heap pointer,		   */
    while (0 < len--)			/* and clear entries in flatspace  */
	flatspace[flatpos++] = NIL;
    return fpr;                         /* return main heap pointer	   */
}

Void overwrite(dst,src)                 /* overwrite dst cell with src cell*/
Cell dst, src; {                        /* both *MUST* be pairs            */
    if (isPair(dst) && isPair(src)) {
	fst(dst) = fst(src);
	snd(dst) = snd(src);
    }
    else
	internal("overwrite");
}

static Int *marks;
static Int marksSize;

Cell markExpr(c)                        /* External interface to markCell  */
Cell c; {
    return isGenPair(c) ? markCell(c) : c;
}

static Cell local markCell(c)           /* Traverse part of graph marking  */
Cell c; {                               /* cells reachable from given root */
					/* markCell(c) is only called if c */
					/* is a pair                       */
mc: switch (fst(c)) {
	case INDIRECT : c = indirectChain(c);
			if (isGenPair(c))
			    goto mc;
			return c;
	case DICTCELL :	moveFlat(c);
			/* intentional fall thru */
	case FLATCELL : return c;
    }

    {   register place = placeInSet(c);
	register mask  = maskInSet(c);
	if (marks[place]&mask)
	    return c;
	else
	    marks[place] |= mask;
    }

    if (isGenPair(fst(c))) {
	fst(c) = markCell(fst(c));
	markSnd(c);
    }
    else if (isNull(fst(c)) || fst(c)>=BCSTAG)
	markSnd(c);

    return c;
}

static Void local markSnd(c)            /* Variant of markCell used to     */
Cell c; {                               /* update snd component of cell    */
    Cell t;                             /* using tail recursion            */

ma: t = c;                              /* Keep pointer to original pair   */
    c = snd(c);
mb: if (!isPair(c))
	return;

    switch (fst(c)) {
	case INDIRECT : snd(t) = c = indirectChain(c);
			goto mb;
	case DICTCELL :	moveFlat(c);
			/* intentional fall thru */
	case FLATCELL : return;
    }

    {   register place = placeInSet(c);
	register mask  = maskInSet(c);
	if (marks[place]&mask)
	    return;
	else
	    marks[place] |= mask;
    }

    if (isGenPair(fst(c))) {
	fst(c) = markCell(fst(c));
	goto ma;
    }
    else if (isNull(fst(c)) || fst(c)>=BCSTAG)
	goto ma;
    return;
}

static Cell local indirectChain(c)      /* Scan chain of indirections      */
Cell c; {                               /* Detecting loops of indirections */
    Cell is = c;                        /* Uses pointer reversal ...       */
    c       = snd(is);
    snd(is) = NIL;
    fst(is) = INDIRECT1;

    while (isPair(c) && fst(c)==INDIRECT) {
	register Cell temp = snd(c);
	snd(c)  = is;
	is      = c;
	c       = temp;
	fst(is) = INDIRECT1;
    }

    if (isPair(c) && fst(c)==INDIRECT1)
	c = nameBlackHole;

    do {
	register Cell temp = snd(is);
	fst(is) = INDIRECT;
	snd(is) = c;
	is      = temp;
    } while (nonNull(is));

    return c;
}

Void markWithoutMove(n)                 /* Garbage collect cell at n, as if*/
Cell n; {                               /* it was a cell ref, but don't    */
					/* move cell (i.e. retain INDIRECT */
					/* at top level) so we don't have  */
					/* to modify the stored value of n */
    if (isGenPair(n)) {
	if (fst(n)==INDIRECT) {         /* special case for indirections   */
	    register place = placeInSet(n);
	    register mask  = maskInSet(n);
	    marks[place]  |= mask;
	    markSnd(n);
	}
	else
	    markCell(n);                /* normal pairs don't move anyway  */
    }
}

static Void local moveFlat(c)		/* Copy flat value into To-space   */
Cell c; {
    Int pos = snd(c);
    Int n   = flatspace[pos] + 2;
    snd(c)  = fst(c);
    fst(c)  = FLATCELL;
    marks[placeInSet(c)] |= maskInSet(c);
    while (0 < n--)
	tospace[topos++] = flatspace[pos++];
}

Void garbageCollect() {                 /* Run garbage collector ...       */
    Bool breakStat = breakOn(FALSE);    /* disable break checking          */
    Int i,j;
    register Int mask;
    register Int place;
    Int      recovered;
    jmp_buf  regs;                      /* save registers on stack         */
    setjmp(regs);

    gcStarted();
    topos = 0;                          /* clear to-space in flat resource */
    for (i=0; i<marksSize; ++i)         /* initialise mark set to empty    */
	marks[i] = 0;

    everybody(MARK);                    /* Mark all components of system   */

    for (flatpos=0; flatpos < topos; ) {/* Scavenge from tospace entries   */
	Int n = tospace[flatpos];
	for (flatpos+=2; 0<n--; flatpos++)
	    mark(tospace[flatpos]);
    }
    for (topos=0; topos < flatpos; ) {	/* ... eliminate FLATCELL tags	   */
	Int  n = tospace[topos];
	Cell c = tospace[topos+1];
	fst(c) = snd(c);
	snd(c) = topos;
	topos += (n+2);
    }
    {   Heap tmp  = flatspace;		/* ... and swap spaces		   */
	flatspace = tospace;
	tospace   = tmp;
    }

#if IO_MONAD
    for (i=0; i<NUM_HANDLES; ++i)       /* release any unused handles      */
	if (nonNull(handles[i].hcell)) {
	    register place = placeInSet(handles[i].hcell);
	    register mask  = maskInSet(handles[i].hcell);
	    if ((marks[place]&mask)==0)
		freeHandle(i);
	}
#endif

    gcScanning();                       /* scan mark set                   */
    mask      = 1;
    place     = 0;
    recovered = 0;
    j         = 0;
#if PROFILING
    if (profile) {
	sysCount = 0;
	for (i=NAMEMIN; i<nameHw; i++)
	    name(i).count = 0;
    }
#endif
    freeList = NIL;
    for (i=1; i<=heapSize; i++) {
	if ((marks[place] & mask) == 0) {
	    snd(-i)  = freeList;
	    fst(-i)  = FREECELL;
	    freeList = -i;
	    recovered++;
	}
#if PROFILING
	else if (nonNull(thd(-i)))
	    name(thd(-i)).count++;
	else
	    sysCount++;
#endif
	mask <<= 1;
	if (++j == bitsPerWord) {
	    place++;
	    mask = 1;
	    j    = 0;
	}
    }

    gcRecovered(recovered,maxFlat-flatpos);
    breakOn(breakStat);                 /* restore break trapping if nec.  */

#if PROFILING
    if (profile) {
	fprintf(profile,"BEGIN_SAMPLE %ld.00\n",numReductions);
/* For the time being, we won't include the system count in the output:
	if (sysCount>0)
	    fprintf(profile,"  SYSTEM %d\n",sysCount);
*/
	for (i=NAMEMIN; i<nameHw; i++)
	    if (name(i).count>0)
		fprintf(profile,"  %s %d\n",
			textToStr(name(i).text),
			name(i).count);
	fprintf(profile,"END_SAMPLE %ld.00\n",numReductions);
    }
#endif

    /* can only return if freeList is nonempty on return. */
    if (recovered<minRecovery || isNull(freeList)) {
	ERRMSG(0) "Garbage collection fails to reclaim sufficient space"
	EEND;
    }
    numberGcs++;
    cellsRecovered = recovered;
}

#if PROFILING
Void profilerLog(s)                     /* turn heap profiling on, saving log*/
String s; {                             /* in specified file                 */
    if (profile=fopen(s,FOPEN_WRITE)) {
	fprintf(profile,"JOB \"Hugs Heap Profile\"\n");
	fprintf(profile,"DATE \"%s\"\n",timeString());
	fprintf(profile,"SAMPLE_UNIT \"reductions\"\n");
	fprintf(profile,"VALUE_UNIT \"cells\"\n");
    }
    else {
	ERRMSG(0) "Cannot open profile log file \"%s\"", s
	EEND;
    }
}
#endif

/* --------------------------------------------------------------------------
 * Code for saving last expression entered:
 *
 * This is a little tricky because some text values (e.g. strings or variable
 * names) may not be defined or have the same value when the expression is
 * recalled.  These text values are therefore saved in the top portion of
 * the text table.
 * ------------------------------------------------------------------------*/

static Cell lastExprSaved;              /* last expression to be saved     */

Void setLastExpr(e)                     /* save expression for later recall*/
Cell e; {
    lastExprSaved = NIL;                /* in case attempt to save fails   */
    savedText     = NUM_TEXT;
    lastExprSaved = lowLevelLastIn(e);
}

static Cell local lowLevelLastIn(c)     /* Duplicate expression tree (i.e. */
Cell c; {                               /* acyclic graph) for later recall */
    if (isPair(c))                      /* Duplicating any text strings    */
	if (isBoxTag(fst(c)))           /* in case these are lost at some  */
	    switch (fst(c)) {           /* point before the expr is reused */
		case VARIDCELL :
		case VAROPCELL :
		case DICTVAR   :
		case CONIDCELL :
		case CONOPCELL :
		case STRCELL   : return pair(fst(c),saveText(textOf(c)));
		default        : return pair(fst(c),snd(c));
	    }
	else
	    return pair(lowLevelLastIn(fst(c)),lowLevelLastIn(snd(c)));
    else
	return c;
}

Cell getLastExpr() {                    /* recover previously saved expr   */
    return lowLevelLastOut(lastExprSaved);
}

static Cell local lowLevelLastOut(c)    /* As with lowLevelLastIn() above  */
Cell c; {                               /* except that Cells refering to   */
    if (isPair(c))                      /* Text values are restored to     */
	if (isBoxTag(fst(c)))           /* appropriate values              */
	    switch (fst(c)) {
		case VARIDCELL :
		case VAROPCELL :
		case DICTVAR   :
		case CONIDCELL :
		case CONOPCELL :
		case STRCELL   : return pair(fst(c),
					     findText(text+intValOf(c)));
		default        : return pair(fst(c),snd(c));
	    }
	else
	    return pair(lowLevelLastOut(fst(c)),lowLevelLastOut(snd(c)));
    else
	return c;
}

/* --------------------------------------------------------------------------
 * Miscellaneous operations on heap cells:
 * ------------------------------------------------------------------------*/

/* profiling suggests that the number of calls to whatIs() is typically    */
/* rather high.  The recoded version below attempts to improve the average */
/* performance for whatIs() using a binary search for part of the analysis */

Cell whatIs(c)                         /* identify type of cell            */
register Cell c; {
    if (isPair(c)) {
	register Cell fstc = fst(c);
	return isTag(fstc) ? fstc : AP;
    }
    if (c<TUPMIN)    return c;
    if (c>=INTMIN)   return INTCELL;

    if (c>=SELMIN)  if (c>=CLASSMIN)    if (c>=CHARMIN) return CHARCELL;
					else            return CLASS;
		    else                if (c>=INSTMIN) return INSTANCE;
					else            return SELECT;
    else            if (c>=TYCMIN)      if (c>=NAMEMIN) return NAME;
					else            return TYCON;
		    else                if (c>=OFFMIN)  return OFFSET;
					else            return TUPLE;

/*  if (c>=CHARMIN)  return CHARCELL;
    if (c>=CLASSMIN) return CLASS;
    if (c>=INSTMIN)  return INSTANCE;
    if (c>=SELMIN)   return SELECT;
    if (c>=NAMEMIN)  return NAME;
    if (c>=TYCMIN)   return TYCON;
    if (c>=OFFMIN)   return OFFSET;
    if (c>=TUPMIN)   return TUPLE;
    return c;*/
}

Bool isVar(c)                           /* is cell a VARIDCELL/VAROPCELL ? */
Cell c; {                               /* also recognises DICTVAR cells   */
    return isPair(c) &&
	       (fst(c)==VARIDCELL || fst(c)==VAROPCELL || fst(c)==DICTVAR);
}

Bool isCon(c)                          /* is cell a CONIDCELL/CONOPCELL ?  */
Cell c; {
    return isPair(c) && (fst(c)==CONIDCELL || fst(c)==CONOPCELL);
}

Bool isInt(c)                          /* cell holds integer value?        */
Cell c; {
    return isSmall(c) || (isPair(c) && fst(c)==INTCELL);
}

Int intOf(c)                           /* find integer value of cell?      */
Cell c; {
    return isPair(c) ? (Int)(snd(c)) : (Int)(c-INTZERO);
}

Cell mkInt(n)                          /* make cell representing integer   */
Int n; {
    return isSmall(INTZERO+n) ? INTZERO+n : pair(INTCELL,n);
}

Bool isBignum(c)                       /* cell holds bignum value?         */
Cell c; {
    return c==ZERONUM || (isPair(c) && (fst(c)==POSNUM || fst(c)==NEGNUM));
}

/* --------------------------------------------------------------------------
 * List operations:
 * ------------------------------------------------------------------------*/

Int length(xs)                         /* calculate length of list xs      */
List xs; {
    Int n = 0;
    for (n=0; nonNull(xs); ++n)
	xs = tl(xs);
    return n;
}

List appendOnto(xs,ys)                 /* Destructively prepend xs onto    */
List xs, ys; {                         /* ys by modifying xs ...           */
    if (isNull(xs))
	return ys;
    else {
	List zs = xs;
	while (nonNull(tl(zs)))
	    zs = tl(zs);
	tl(zs) = ys;
	return xs;
    }
}

List dupList(xs)                       /* Duplicate spine of list xs       */
List xs; {
    List ys = NIL;
    for (; nonNull(xs); xs=tl(xs))
	ys = cons(hd(xs),ys);
    return rev(ys);
}

List revOnto(xs,ys)                    /* Destructively reverse elements of*/
List xs, ys; {                         /* list xs onto list ys...          */
    Cell zs;

    while (nonNull(xs)) {
	zs     = tl(xs);
	tl(xs) = ys;
	ys     = xs;
	xs     = zs;
    }
    return ys;
}

Cell varIsMember(t,xs)                 /* Test if variable is a member of  */
Text t;                                /* given list of variables          */
List xs; {
    for (; nonNull(xs); xs=tl(xs))
	if (t==textOf(hd(xs)))
	    return hd(xs);
    return NIL;
}

Cell intIsMember(n,xs)                 /* Test if integer n is member of   */
Int  n;                                /* given list of integers           */
List xs; {
    for (; nonNull(xs); xs=tl(xs))
	if (n==intOf(hd(xs)))
	    return hd(xs);
    return NIL;
}

Cell cellIsMember(x,xs)                /* Test for membership of specific  */
Cell x;                                /* cell x in list xs                */
List xs; {
    for (; nonNull(xs); xs=tl(xs))
	if (x==hd(xs))
	    return hd(xs);
    return NIL;
}

List copy(n,x)                         /* create list of n copies of x     */
Int n;
Cell x; {
    List xs=NIL;
    while (0<n--)
	xs = cons(x,xs);
    return xs;
}

List diffList(from,take)               /* list difference: from\take       */
List from, take; {                     /* result contains all elements of  */
    List result = NIL;                 /* `from' not appearing in `take'   */

    while (nonNull(from)) {
	List next = tl(from);
	if (!cellIsMember(hd(from),take)) {
	    tl(from) = result;
	    result   = from;
	}
	from = next;
    }
    return rev(result);
}

List take(n,xs)                         /* destructively truncate list to  */
Int  n;                                 /* specified length                */
List xs; {
    List start = xs;

    if (n==0)
	return NIL;
    while (1<n-- && nonNull(xs))
	xs = tl(xs);
    if (nonNull(xs))
	tl(xs) = NIL;
    return start;
}

List initSeg(xs)                        /* destructively truncate list to  */
List xs; {                              /* its initial segment             */
    if (isNull(xs) || isNull(tl(xs)))
	return NIL;
    else
	return take(length(xs)-1,xs);   /* not the best, but it'll do      */
}

List removeCell(x,xs)                   /* destructively remove cell from  */
Cell x;                                 /* list                            */
List xs; {
    if (nonNull(xs)) {
	if (hd(xs)==x)
	    return tl(xs);              /* element at front of list        */
	else {
	    List prev = xs;
	    List curr = tl(xs);
	    for (; nonNull(curr); prev=curr, curr=tl(prev))
		if (hd(curr)==x) {
		    tl(prev) = tl(curr);
		    return xs;          /* element in middle of list       */
		}
	}
    }
    return xs;                          /* here if element not found       */
}

/* --------------------------------------------------------------------------
 * Operations on applications:
 * ------------------------------------------------------------------------*/

Int argCount;                          /* number of args in application    */

Cell getHead(e)                        /* get head cell of application     */
Cell e; {                              /* set number of args in argCount   */
    for (argCount=0; isAp(e); e=fun(e))
	argCount++;
    return e;
}

List getArgs(e)                        /* get list of arguments in function*/
Cell e; {                              /* application:                     */
    List as;                           /* getArgs(f e1 .. en) = [e1,..,en] */

    for (as=NIL; isAp(e); e=fun(e))
	as = cons(arg(e),as);
    return as;
}

Cell nthArg(n,e)                       /* return nth arg in application    */
Int  n;                                /* of function to m args (m>=n)     */
Cell e; {                              /* nthArg n (f x0 x1 ... xm) = xn   */
    for (n=numArgs(e)-n-1; n>0; n--)
	e = fun(e);
    return arg(e);
}

Int numArgs(e)                         /* find number of arguments to expr */
Cell e; {
    Int n;
    for (n=0; isAp(e); e=fun(e))
	n++;
    return n;
}

Cell applyToArgs(f,args)               /* destructively apply list of args */
Cell f;                                /* to function f                    */
List args; {
    while (nonNull(args)) {
	Cell temp = tl(args);
	tl(args)  = hd(args);
	hd(args)  = f;
	f         = args;
	args      = temp;
    }
    return f;
}

/* --------------------------------------------------------------------------
 * Handle operations:
 * ------------------------------------------------------------------------*/

#if IO_MONAD
struct strHandle DEFTABLE(handles,NUM_HANDLES);

Cell openHandle(s,hmode)                /* open handle to file named s in  */
String s;                               /* the specified hmode             */
Int    hmode; {
    Int i;

    for (i=0; i<NUM_HANDLES && nonNull(handles[i].hcell); ++i)
	;                                       /* Search for unused handle*/
    if (i>=NUM_HANDLES) {                       /* If at first we don't    */
	garbageCollect();                       /* succeed, garbage collect*/
	for (i=0; i<NUM_HANDLES && nonNull(handles[i].hcell); ++i)
	    ;                                   /* and try again ...       */
    }
    if (i>=NUM_HANDLES) {                       /* ... before we give up   */
	ERRMSG(0) "Too many handles open; cannot open \"%s\"", s
	EEND;
    }
    else {                                      /* prepare to open file    */
	String stmode = (hmode&HAPPEND) ? FOPEN_APPEND :
			(hmode&HWRITE)  ? FOPEN_WRITE  :
			(hmode&HREAD)   ? FOPEN_READ   : 0;
	if (stmode && (handles[i].hfp=fopen(s,stmode))) {
	    handles[i].hmode = hmode;
	    return (handles[i].hcell = ap(HANDCELL,i));
	}
    }
    return NIL;
}

static Void local freeHandle(n)         /* release handle storage when no  */
Int n; {                                /* heap references to it remain    */
    if (0<=n && n<NUM_HANDLES && nonNull(handles[n].hcell)) {
	if (n>HSTDERR && handles[n].hmode!=HCLOSED && handles[n].hfp) {
	    fclose(handles[n].hfp);
	    handles[n].hfp = 0;
	}
	fst(handles[n].hcell) = snd(handles[n].hcell) = NIL;
	handles[n].hcell      = NIL;
    }
}
#endif

/* --------------------------------------------------------------------------
 * storage control:
 * ------------------------------------------------------------------------*/

#if DYN_TABLES
static void far* safeFarCalloc Args((Int,Int));
static void far* safeFarCalloc(n,s)     /* allocate table storage and check*/
Int n, s; {                             /* for non-null return             */
    void far* tab = farCalloc(n,s);
    if (tab==0) {
	ERRMSG(0) "Cannot allocate run-time tables"
	EEND;
    }
    return tab;
}
#define TABALLOC(v,t,n)                 v=(t far*)safeFarCalloc(n,sizeof(t));
#else
#define TABALLOC(v,t,n)
#endif

Void storage(what)
Int what; {
    Int i;

    switch (what) {
	case RESET   : clearStack();

		       /* the next 2 statements are particularly important
			* if you are using GLOBALfst or GLOBALsnd since the
			* corresponding registers may be reset to their
			* uninitialised initial values by a longjump.
			*/
		       heapTopFst = heapFst + heapSize;
		       heapTopSnd = heapSnd + heapSize;
#if PROFILING
		       heapTopThd = heapThd + heapSize;
		       if (profile) {
			   garbageCollect();
			   fclose(profile);
#if UNIX
			   system("hp2ps profile.hp");
#endif
			   profile = 0;
		       }
#endif
#if IO_MONAD
		       handles[HSTDIN].hmode  = HREAD;
		       handles[HSTDOUT].hmode = HAPPEND;
		       handles[HSTDERR].hmode = HAPPEND;
#endif
		       consGC = TRUE;
		       lsave  = NIL;
		       rsave  = NIL;
		       if (isNull(lastExprSaved))
			   savedText = NUM_TEXT;
		       break;

	case MARK    : for (i=TYCMIN; i<tyconHw; ++i) {
			   mark(tycon(i).defn);
			   mark(tycon(i).kind);
			   mark(tycon(i).what);
		       }

		       for (i=NAMEMIN; i<nameHw; ++i) {
			   mark(name(i).defn);
			   mark(name(i).type);
		       }

		       for (i=CLASSMIN; i<classHw; ++i) {
			   mark(cclass(i).sig);
			   mark(cclass(i).supers);
			   mark(cclass(i).members);
			   mark(cclass(i).defaults);
			   mark(cclass(i).instances);
		       }

		       for (i=INSTMIN; i<instHw; ++i) {
			   mark(inst(i).specifics);
			   mark(inst(i).implements);
			   mark(inst(i).dicts);
			   mark(inst(i).superBuild);
		       }

		       for (i=0; i<=sp; ++i)
			   mark(stack(i));

		       mark(lastExprSaved);
		       mark(lsave);
		       mark(rsave);
		       mark(handles[HSTDIN].hcell);
		       mark(handles[HSTDOUT].hcell);
		       mark(handles[HSTDERR].hcell);

		       if (consGC)
			   gcCStack();

		       break;

	case INSTALL : heapFst = heapAlloc(heapSize);
		       heapSnd = heapAlloc(heapSize);

		       if (heapFst==(Heap)0 || heapSnd==(Heap)0) {
			   ERRMSG(0) "Cannot allocate heap storage (%d cells)",
				     heapSize
			   EEND;
		       }

		       heapTopFst = heapFst + heapSize;
		       heapTopSnd = heapSnd + heapSize;
#if PROFILING
		       heapThd = heapAlloc(heapSize);
		       if (heapThd==(Heap)0) {
			   ERRMSG(0) "Cannot allocate profiler storage space"
			   EEND;
		       }
		       heapTopThd   = heapThd + heapSize;
		       profile      = 0;
		       profInterval = heapSize / DEF_PROFINTDIV;
#endif
		       for (i=1; i<heapSize; ++i) {
		           fst(-i) = FREECELL;
			   snd(-i) = -(i+1);
		       }
		       snd(-heapSize) = NIL;
		       freeList  = -1;
		       numberGcs = 0;
		       consGC    = TRUE;
		       lsave     = NIL;
		       rsave     = NIL;

		       marksSize  = bitArraySize(heapSize);
		       if ((marks=(Int *)calloc(marksSize, sizeof(Int)))==0) {
			   ERRMSG(0) "Unable to allocate gc markspace"
			   EEND;
		       }

		       flatspace = heapAlloc(maxFlat);
		       tospace   = heapAlloc(maxFlat);
		       flatpos   = 0;
		       if (flatspace==(Heap)0 || tospace==(Heap)0) {
			   ERRMSG(0) "Cannot allocate flat space (%d cells)",
				     maxFlat
			   EEND;
		       }

		       TABALLOC(text,      char,             NUM_TEXT)
		       TABALLOC(tabSyntax, struct strSyntax, NUM_SYNTAX)
		       TABALLOC(tyconHash, Tycon,            TYCONHSZ)
		       TABALLOC(tabTycon,  struct strTycon,  NUM_TYCON)
		       TABALLOC(nameHash,  Name,             NAMEHSZ)
		       TABALLOC(tabName,   struct strName,   NUM_NAME)
		       TABALLOC(tabClass,  struct strClass,  NUM_CLASSES)
		       TABALLOC(cellStack, Cell,             NUM_STACK)
		       TABALLOC(modules,   module,           NUM_SCRIPTS)
		       clearStack();

#if IO_MONAD
		       TABALLOC(handles,   struct strHandle, NUM_HANDLES)
		       for (i=0; i<NUM_HANDLES; i++)
			   handles[i].hcell = NIL;
		       handles[HSTDIN].hcell  = ap(HANDCELL,HSTDIN);
		       handles[HSTDIN].hfp    = stdin;
		       handles[HSTDOUT].hcell = ap(HANDCELL,HSTDOUT);
		       handles[HSTDOUT].hfp   = stdout;
		       handles[HSTDERR].hcell = ap(HANDCELL,HSTDERR);
		       handles[HSTDERR].hfp   = stderr;
#endif

		       textHw        = 0;
		       nextNewText   = NUM_TEXT;
		       nextNewDText  = (-1);
		       lastExprSaved = NIL;
		       savedText     = NUM_TEXT;
		       for (i=0; i<TEXTHSZ; ++i)
			   textHash[i][0] = NOTEXT;

		       syntaxHw = 0;

		       addrHw   = 0;

		       tyconHw  = TYCMIN;
		       for (i=0; i<TYCONHSZ; ++i)
			   tyconHash[i] = NIL;

		       nameHw = NAMEMIN;
		       for (i=0; i<NAMEHSZ; ++i)
			   nameHash[i] = NIL;

		       classHw  = CLASSMIN;

		       instHw   = INSTMIN;

		       tabInst  = (struct strInst far *)
				    farCalloc(NUM_INSTS,sizeof(struct strInst));

		       if (tabInst==0) {
			   ERRMSG(0) "Cannot allocate instance tables"
			   EEND;
		       }

		       moduleHw = 0;

		       break;
    }
}

/*-------------------------------------------------------------------------*/
