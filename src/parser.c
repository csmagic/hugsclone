/* A YACC parser generated from "parser.y" */

# line 14
#ifndef lint
#define lint
#endif
#define defTycon(n,l,lhs,rhs,w)  tyconDefn(intOf(l),lhs,rhs,w); sp-=n
#define sigdecl(l,vs,t)          ap(SIGDECL,triple(l,vs,t))
#define grded(gs)                ap(GUARDED,gs)
#define bang(t)                  ap(BANG,t)
#define letrec(bs,e)             (nonNull(bs) ? ap(LETREC,pair(bs,e)) : e)
#define yyerror(s)               /* errors handled elsewhere */
#define YYSTYPE                  Cell

static Cell   local gcShadow     Args((Int,Cell));
static Void   local syntaxError  Args((String));
static String local unexpected   Args((Void));
static Cell   local checkPrec    Args((Cell));
static Void   local fixDefn      Args((Syntax,Cell,Cell,List));
static Void   local setSyntax    Args((Int,Syntax,Cell));
static Cell   local buildTuple   Args((List));
static List   local tupToList    Args((Cell));
static List   local checkContext Args((List));
static Cell   local checkClass   Args((Cell));
static Cell   local checkInst    Args((Cell));
static Pair   local checkDo      Args((List));
static Cell   local checkTyLhs   Args((Cell));
static Cell   local tidyInfix    Args((Cell));

/* For the purposes of reasonably portable garbage collection, it is
 * necessary to simulate the YACC stack on the Hugs stack to keep
 * track of all intermediate constructs.  The lexical analyser
 * pushes a token onto the stack for each token that is found, with
 * these elements being removed as reduce actions are performed,
 * taking account of look-ahead tokens as described by gcShadow()
 * below.
 *
 * Of the non-terminals used below, only start, topDecl, fixDecl & begin
 * do not leave any values on the Hugs stack.  The same is true for the
 * terminals EXPR and SCRIPT.  At the end of a successful parse, there
 * should only be one element left on the stack, containing the result
 * of the parse.
 */

#define gc0(e)                   gcShadow(0,e)
#define gc1(e)                   gcShadow(1,e)
#define gc2(e)                   gcShadow(2,e)
#define gc3(e)                   gcShadow(3,e)
#define gc4(e)                   gcShadow(4,e)
#define gc5(e)                   gcShadow(5,e)
#define gc6(e)                   gcShadow(6,e)
#define gc7(e)                   gcShadow(7,e)

#define EXPR 257
#define SCRIPT 258
#define CASEXP 259
#define OF 260
#define DATA 261
#define TYPE 262
#define IF 263
#define THEN 264
#define ELSE 265
#define WHERE 266
#define LET 267
#define IN 268
#define INFIX 269
#define INFIXL 270
#define INFIXR 271
#define PRIMITIVE 272
#define TNEWTYPE 273
#define DEFAULT 274
#define DERIVING 275
#define DO 276
#define TCLASS 277
#define TINSTANCE 278
#define TRUNST 279
#define REPEAT 280
#define VAROP 281
#define VARID 282
#define NUMLIT 283
#define CHARLIT 284
#define STRINGLIT 285
#define CONOP 286
#define CONID 287
#define COCO 288
#define UPTO 289
#define FROM 290
#define ARROW 291
#define IMPLIES 292
#define MODULE 293
#define IMPORT 294
#define HIDING 295
#define QUALIFIED 296
#define ASMOD 297
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

#line 560


static Cell local gcShadow(n,e)         /* keep parsed fragments on stack  */
Int  n;
Cell e; {
    /* If a look ahead token is held then the required stack transformation
     * is:
     *   pushed: n               1     0          1     0
     *           x1  |  ...  |  xn  |  la   ===>  e  |  la
     *                                top()            top()
     *
     * Othwerwise, the transformation is:
     *   pushed: n-1             0        0
     *           x1  |  ...  |  xn  ===>  e
     *                         top()     top()
     */
    if (yychar>=0) {
	pushed(n-1) = top();
	pushed(n)   = e;
    }
    else
	pushed(n-1) = e;
    sp -= (n-1);
    return e;
}

static Void local syntaxError(s)       /* report on syntax error           */
String s; {
    ERRMSG(row) "Syntax error in %s (unexpected %s)", s, unexpected()
    EEND;
}

static String local unexpected() {      /* find name for unexpected token  */
    static char buffer[100];
    static char *fmt = "%s \"%s\"";
    static char *kwd = "keyword";

    switch (yychar) {
	case 0         : return "end of input";

#define keyword(kw) sprintf(buffer,fmt,kwd,kw); return buffer;
	case INFIXL    : keyword("infixl");
	case INFIXR    : keyword("infixr");
	case INFIX     : keyword("infix");
	case TINSTANCE : keyword("instance");
	case TCLASS    : keyword("class");
	case PRIMITIVE : keyword("primitive");
	case CASEXP    : keyword("case");
	case OF        : keyword("of");
	case IF        : keyword("if");
	case TRUNST    : keyword("runST");
	case THEN      : keyword("then");
	case ELSE      : keyword("else");
	case WHERE     : keyword("where");
	case TYPE      : keyword("type");
	case DATA      : keyword("data");
	case TNEWTYPE  : keyword("newtype");
	case LET       : keyword("let");
	case IN        : keyword("in");
	case DERIVING  : keyword("deriving");
	case DEFAULT   : keyword("default");
	case IMPORT    : keyword("import");
	case MODULE    : keyword("module");
#undef keyword

	case ARROW     : return "`->'";
	case '='       : return "`='";
	case COCO      : return "`::'";
	case '-'       : return "`-'";
	case '!'       : return "`!'";
	case ','       : return "comma";
	case '@'       : return "`@'";
	case '('       : return "`('";
	case ')'       : return "`)'";
	case '|'       : return "`|'";
	case ';'       : return "`;'";
	case UPTO      : return "`..'";
	case '['       : return "`['";
	case ']'       : return "`]'";
	case FROM      : return "`<-'";
	case '\\'      : return "backslash (lambda)";
	case '~'       : return "tilde";
	case '`'       : return "backquote";
	case VAROP     :
	case VARID     :
	case CONOP     :
	case CONID     : sprintf(buffer,"symbol \"%s\"",
				 textToStr(textOf(yylval)));
			 return buffer;
	case HIDING    : return "symbol \"hiding\"";
	case QUALIFIED : return "symbol \"qualified\"";
	case ASMOD     : return "symbol \"as\"";
	case NUMLIT    : return "numeric literal";
	case CHARLIT   : return "character literal";
	case STRINGLIT : return "string literal";
	case IMPLIES   : return "`=>'";
	default        : return "token";
    }
}

static Cell local checkPrec(p)         /* Check for valid precedence value */
Cell p; {
    if (!isInt(p) || intOf(p)<MIN_PREC || intOf(p)>MAX_PREC) {
	ERRMSG(row) "Precedence value must be an integer in the range [%d..%d]",
		    MIN_PREC, MAX_PREC
	EEND;
    }
    return p;
}

static Void local fixDefn(a,line,p,ops)/* Declare syntax of operators      */
Syntax a;
Cell   line;
Cell   p;
List   ops; {
    Int l = intOf(line);
    a     = mkSyntax(a,intOf(p));
    map2Proc(setSyntax,l,a,ops);
}

static Void local setSyntax(line,sy,op)/* set syntax of individ. operator  */
Int    line;
Syntax sy;
Cell   op; {
    addSyntax(line,textOf(op),sy);
    opDefns = cons(op,opDefns);
}

static Cell local buildTuple(tup)      /* build tuple (x1,...,xn) from list*/
List tup; {                            /* [xn,...,x1]                      */
    Int  n = 0;
    Cell t = tup;
    Cell x;

    do {                               /*     .                    .       */
	x      = fst(t);               /*    / \                  / \      */
	fst(t) = snd(t);               /*   xn  .                .   xn    */
	snd(t) = x;                    /*        .    ===>      .          */
	x      = t;                    /*         .            .           */
	t      = fun(x);               /*          .          .            */
	n++;                           /*         / \        / \           */
    } while (nonNull(t));              /*        x1  NIL   (n)  x1         */
    fst(x) = mkTuple(n);
    return tup;
}

/* The yacc parser presented above is not sufficiently powerful to
 * determine whether a tuple at the front of a sigType is part of a
 * context:    e.g. (Eq a, Num a) => a -> a -> a
 * or a type:  e.g.  (Tree a, Tree a) -> Tree a
 *
 * Rather than complicate the grammar, both are parsed as tuples of types,
 * using the following checks afterwards to ensure that the correct syntax
 * is used in the case of a tupled context.
 */

static List local tupToList(tps)        /* Convert () | t | (t1,...,tn) to */
Cell tps; {                             /* a list of values:               */
    if (tps==typeUnit)                  /*        [] | [t] | [t1,...,tn]   */
	return NIL;
    else if (whatIs(getHead(tps))==TUPLE) {
	List qs = NIL;

	while (isAp(tps)) {             /* undo work of buildTuple  :-(    */
	    Cell temp = fun(tps);
	    fun(tps)  = arg(tps);
	    arg(tps)  = qs;
	    qs        = tps;
	    tps       = temp;
	}
	return qs;
    }
    else
	return singleton(tps);
}

static List local checkContext(con)     /* validate type class context     */
Type con; {
    mapOver(checkClass, con=tupToList(con));
    return con;
}

static Cell local checkClass(c)         /* check that type expr is a class */
Cell c; {                               /* constrnt of the form Class var  */
    Cell cn = getHead(c);

    if (!isCon(cn))
	syntaxError("class expression");
    else if (argCount!=1) {
	ERRMSG(row) "Class \"%s\" must have exactly one argument",
		    textToStr(textOf(cn))
	EEND;
    }
    else if (whatIs(arg(c))!=VARIDCELL) {
	ERRMSG(row) "Argument of class \"%s\" must be a variable",
		    /* Ha!  What do you think this is?  Gofer!? :-) */
		    textToStr(textOf(cn))
	EEND;
    }
    return c;
}

static Cell local checkInst(c)          /* check that type expr is a class */
Cell c; {                               /* constr of the form Class simple */
    Cell cn = getHead(c);

    if (!isCon(cn))
	syntaxError("class expression");
    else if (argCount!=1) {
	ERRMSG(row) "Class \"%s\" must have exactly one argument",
		    textToStr(textOf(cn))
	EEND;
    }
    else {
	Cell a  = arg(c);
	Cell tn = getHead(a);
	if (isCon(tn) || isTycon(tn) || isTuple(tn)) {
	    for (; isAp(a); a=fun(a))
		if (whatIs(arg(a))!=VARIDCELL) {
		    ERRMSG(row) "Type variable expected in instance type"
		    EEND;
		}
	}
	else {
	    ERRMSG(row) "Illegal type expression in instance declaration"
	    EEND;
	}
    }
    return c;
}

static Pair local checkDo(dqs)          /* convert reversed list of dquals */
List dqs; {                             /* to an (expr,quals) pair         */
    if (isNull(dqs) || whatIs(hd(dqs))!=DOQUAL) {
	ERRMSG(row) "Last generator in do {...} must be an expression"
	EEND;
    }
    fst(dqs) = snd(fst(dqs));           /* put expression in fst of pair   */
    snd(dqs) = rev(snd(dqs));           /* & reversed list of quals in snd */
    return dqs;
}

static Cell local checkTyLhs(c)         /* check that lhs is of the form   */
Cell c; {                               /* T a1 ... a                      */
    Cell tlhs = c;
    while (isAp(tlhs) && whatIs(arg(tlhs))==VARIDCELL)
	tlhs = fun(tlhs);
    if (whatIs(tlhs)!=CONIDCELL) {
	ERRMSG(row) "Illegal left hand side in type definition"
	EEND;
    }
    return c;
}

/* expressions involving a sequence of two or more infix operator symbols
 * are parsed as elements of type:
 *    InfixExpr ::= [Expr]
 *               |  ap(ap(Operator,InfixExpr),Expr)
 *
 * thus x0 +1 x1 ... +n xn is parsed as: +n (....(+1 [x0] x1)....) xn
 *
 * Once the expression has been completely parsed, this parsed form is
 * `tidied' according to the precedences and associativities declared for
 * each operator symbol.
 *
 * The tidy process uses a `stack' of type:
 *    TidyStack ::= ap(ap(Operator,TidyStack),Expr)
 *               |  NIL
 * when the ith layer of an InfixExpr has been transferred to the stack, the
 * stack is of the form: +i (....(+n NIL xn)....) xi
 *
 * The tidy function is based on a simple shift-reduce parser:
 *
 *  tidy                :: InfixExpr -> TidyStack -> Expr
 *  tidy [m]   ss        = foldl (\x f-> f x) m ss
 *  tidy (m*n) []        = tidy m [(*n)]
 *  tidy (m*n) ((+o):ss)
 *             | amb     = error "Ambiguous"
 *             | shift   = tidy m ((*n):(+o):ss)
 *             | reduce  = tidy (m*(n+o)) ss
 *                         where sye     = syntaxOf (*)
 *                               (ae,pe) = sye
 *                               sys     = syntaxOf (+)
 *                               (as,ps) = sys
 *                               amb     = pe==ps && (ae/=as || ae==NON_ASS)
 *                               shift   = pe>ps || (ps==pe && ae==LEFT_ASS)
 *                               reduce  = otherwise
 *
 * N.B. the conditions amb, shift, reduce are NOT mutually exclusive and
 * must be tested in that order.
 *
 * As a concession to efficiency, we lower the number of calls to syntaxOf
 * by keeping track of the values of sye, sys throughout the process.  The
 * value APPLIC is used to indicate that the syntax value is unknown.
 */

static Cell local tidyInfix(e)         /* convert InfixExpr to Expr        */
Cell e; {                              /* :: InfixExpr                     */
    Cell   s   = NIL;                  /* :: TidyStack                     */
    Syntax sye = APPLIC;               /* Syntax of op in e (init unknown) */
    Syntax sys = APPLIC;               /* Syntax of op in s (init unknown) */
    Cell   temp;

    while (nonNull(tl(e))) {
	if (isNull(s)) {
	    s           = e;
	    e           = arg(fun(s));
	    arg(fun(s)) = NIL;
	    sys         = sye;
	    sye         = APPLIC;
	}
	else {
	    if (sye==APPLIC) {         /* calculate sye (if unknown)       */
		sye = syntaxOf(textOf(fun(fun(e))));
		if (sye==APPLIC) sye=DEF_OPSYNTAX;
	    }
	    if (sys==APPLIC) {         /* calculate sys (if unknown)       */
		sys = syntaxOf(textOf(fun(fun(s))));
		if (sys==APPLIC) sys=DEF_OPSYNTAX;
	    }

	    if (precOf(sye)==precOf(sys) &&                      /* amb    */
		   (assocOf(sye)!=assocOf(sys) || assocOf(sye)==NON_ASS)) {
		ERRMSG(row) "Ambiguous use of operator \"%s\" with \"%s\"",
			    textToStr(textOf(fun(fun(e)))),
			    textToStr(textOf(fun(fun(s))))
		EEND;
	    }
	    else if (precOf(sye)>precOf(sys) ||                  /* shift  */
		       (precOf(sye)==precOf(sys) && assocOf(sye)==LEFT_ASS)) {
		temp        = arg(fun(e));
		arg(fun(e)) = s;
		s           = e;
		e           = temp;
		sys         = sye;
		sye         = APPLIC;
	    }
	    else {                                               /* reduce */
		temp        = arg(fun(s));
		arg(fun(s)) = arg(e);
		arg(e)      = s;
		s           = temp;
		sys         = APPLIC;
		/* sye unchanged */
	    }
	}
    }

    e = hd(e);
    while (nonNull(s)) {
	temp        = arg(fun(s));
	arg(fun(s)) = e;
	e           = s;
	s           = temp;
    }

    return e;
}

/*-------------------------------------------------------------------------*/
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 107,
	288, 182,
	44, 182,
	-2, 215,
-1, 117,
	292, 138,
	-2, 137,
-1, 188,
	292, 138,
	-2, 82,
-1, 198,
	292, 138,
	-2, 129,
-1, 201,
	292, 138,
	-2, 131,
-1, 345,
	123, 192,
	-2, 147,
	};
#define YYNPROD 277
# define YYLAST 1345
short yyact[]={

  32, 410, 437, 105, 385, 256, 424, 117,   5,  23,
 430, 118, 348, 121, 115,  48,  85, 171, 338, 120,
  60,  61, 316, 297, 271, 269, 122, 337,  84, 216,
  74, 330, 298,  90,  33,  82, 255, 184,  97, 130,
  78, 266,  40,  42, 271, 374, 124,  34,  35,  36,
 285,  46,  54, 129,  20, 283,  43, 373, 278, 276,
 214, 446, 215, 112, 168, 324, 113, 238, 111, 204,
 246, 112,  51, 322, 113, 448, 111, 183, 280,  42,
  45, 468, 244, 451,  49, 204, 110, 350, 187, 109,
 130, 396, 428, 107, 281, 179, 386, 246, 387, 384,
 124, 124, 134, 124, 124, 193,  44, 188, 190, 244,
 198, 201, 112, 390, 107, 113, 295, 111, 162, 186,
 150, 124, 124, 150, 313, 139, 124, 124, 140,   4,
   2,   3, 149, 222, 225,  52, 208,  92, 230, 172,
 231, 247, 374, 143, 169, 234,   8, 217, 217, 455,
 211, 219, 240, 232, 241, 252, 195, 251, 395, 236,
 310, 359, 229,  59, 210, 248,  65, 249, 235,  86,
 227,  68,  91,  62, 253, 178, 258, 372, 228, 167,
  17,  66, 147, 185, 358, 106, 274, 182, 277, 275,
 243, 259,  55, 378, 368, 237, 233,   6, 177, 211,
 176, 174, 360, 124, 124, 361, 106, 464, 260, 447,
 288, 445, 371, 382, 292, 124, 124, 294, 289, 357,
  58, 308, 302, 303, 309, 307, 328, 323, 165, 107,
 454, 242, 107, 305, 151, 400, 306,  65, 463,  65,
 166,  24, 161, 165, 319, 160, 444, 387, 161, 321,
  55, 153, 175, 282, 158, 443, 284, 159, 157, 290,
 144, 420, 413, 291, 381, 114, 408, 356, 300, 156,
 173, 180, 181, 272, 124, 392, 124, 155, 326, 270,
 124, 336, 125, 363, 124, 343, 124, 349, 107, 341,
 154, 354,  29, 355, 352, 340,  22, 150, 335, 270,
 304, 334, 329,  53, 333, 213, 151, 124, 202, 170,
 124, 254, 265, 456, 364, 263, 107, 365, 245, 366,
  50, 106, 279, 396, 106,  53, 300,  21, 375, 203,
 377, 207, 367, 126,  31, 195, 413, 127, 390, 427,
 124, 124, 124, 124, 148, 245, 142, 124, 274, 262,
 124, 388, 274, 398, 222, 151, 389, 391, 124, 331,
 376, 380, 333, 370, 402, 401, 239, 217, 217, 217,
 379, 219, 394, 412, 293, 320, 217, 417, 151, 399,
 106, 318, 317, 125, 300, 405, 353, 124, 403, 362,
 124, 414, 124, 124, 124, 419, 343, 369, 124, 438,
 341, 432, 433, 432, 429, 426, 340, 343, 106, 422,
 349, 341, 260, 441, 107, 421, 300, 340, 411, 217,
 217, 442, 434, 435, 383, 440, 415, 397, 416, 261,
 124, 327, 124, 107, 126, 144, 226, 300, 127, 425,
 452, 432, 116, 453,  75, 457, 458, 459, 223, 124,
 439, 315, 460,  81, 461, 412,  33, 124, 465, 438,
 347,  24, 351, 462, 466, 431,  11,  41, 124,  34,
  35,  36, 125,  79,  83, 469,  33, 141, 325, 314,
  88,  28,  56,  33,  25,  26,  27,  10,  31,  34,
  35,  36, 132, 267, 268,  47,  34,  35,  36,  30,
 411, 209, 425,  24,  33, 206, 106, 108,  11,  31,
 439, 406,  29,  12, 205, 317,  22,  34,  35,  36,
  77, 418,  88, 126,  33, 106, 137, 127,  53, 218,
  24, 135, 224, 287, 286,  11, 199,  34,  35,  36,
 196, 192, 189, 191, 273, 197, 200,  21,  33, 450,
 125, 449, 436, 301,  29,  12, 342, 423,  22, 299,
  89,  34,  35,  36, 409,  71, 407, 332, 296,  87,
 212,  39,  24,  73,  38,  37,  80,  69,  33,   1,
   0,  29,  12,  31,   0,  22,   0,   0,   0,  21,
   0,  34,  35,  36, 194,   0,   0,  33,   0, 119,
   0, 126, 301,   0,   0, 127, 390,   0,   0,   0,
  34,  35,  36,   0,   0,   0,  21,   0,   0,   9,
  33,  24,   0,  29,  12,  33,  11,  22,  52,   0,
 123,   0,   0,  34,  35,  36,   0,   0,  34,  35,
  36,   0,   0,   0,  76,   0,   0,   0,  33,   0,
   0,   0,  93,   0,   0, 151, 404,   0,  21,   0,
   0,  34,  35,  36,   0,   0, 128, 151,   0,   0,
   0,   0,  29,  12, 131,   0,  22,   0,   0,   0,
  15,   0,  99,  98,  14,   0,   0,   0,  13,   0,
  96,  94,  95, 101, 100, 104,   0,  16, 102, 103,
  18,  28,   0,  33,  25,  26,  27,  21,  31,   0,
   0,   0, 138,   0,  33,  97,  34,  35,  36, 123,
   0,   0,  15,   0,  99,  98,  14,  34,  35,  36,
  13,   0,  96,  94,  95, 101, 100, 104,   0,  16,
 102, 103,  18,  28,   0,  33,  25,  26,  27,  15,
  31,  99,  98,  14,   0,   0,   0,  13,  34,  35,
  36,   0, 101, 100, 104,  33,  16, 102, 103,  18,
  28,   0,  33,  25,  26,  27,   0,  31,  34,  35,
  36,   0, 128, 467,  24,  34,  35,  36,   7,  11,
 125,  15,  33,   0,   0,  14,  53, 218,   0,  13,
   0,   0,   0,   0,   0,  34,  35,  36,  16,   0,
   0,  18,  28,  70,  33,  25,  26,  27,  72,  31,
   0,   0,   0,   0,   0,   0,   0,  34,  35,  36,
 264,   0,   0,  24, 250,  29,  12,   7,  11,  22,
  15, 126,   0,   0,  14, 127,   0, 311,  13, 125,
 220,   0, 125,  80,   0,   0,   0,  16,   0,   0,
  18,  28,   0,  33,  25,  26,  27,   0,  31,   0,
  21,   0,   0,   0,  24,   0,  34,  35,  36,  11,
   0,   0,   0,   0,  29,  12, 312,   0,  22,   0,
   0,   0,   0, 136,   0,   0,   0,  33,   0,   0,
 126,  24, 301, 126, 127,   0,  11, 127, 299,  33,
  34,  35,  36,   0, 301,   0,   0,  19,   0,  21,
   0,   0,  34,  35,  36,  29,  12,   0,  24,  22,
  57,   0,   0,  11,   0,  63,  64,   0,   0,  67,
   0,   0,   0,   0,   0,   0,   0,  24, 154,   0,
   0,   0,  29,  12,   0,   0,  22,   0,   0,   0,
  21,   0,   0,   0,   0,   0,   0,  24,   0,   0,
   0,   0,   0,  63, 133,   0,   0,   0,   0,  29,
  12,   0,   0,  22, 152,   0,   0,  21,   0,   0,
   0,   0,   0,  24,   0, 163, 164,   0,  29,   0,
   7,   0,  22,  15, 125, 220, 119,  14,  80, 339,
   0, 257,   0,   0,  21,   0, 346,   0,  29,   0,
  16,   0,  22,  18,  28,   0,  33,  25,  26,  27,
   0,  31,  33,  21,   0,   0,   0, 123,   0,  34,
  35,  36,   0,   0,  29,  34,  35,  36,  22,   0,
   0,   0,  15,  21,   0, 126,  14,   0,   0, 127,
  13,   0,   0,   0,   0, 119,   0, 126, 119,  16,
   0, 127,  18,  28, 393,  33,  25,  26,  27,  21,
  31, 125,   0,   0, 125,   0,   0, 125,  34,  35,
  36,  33,   0,  15,  33, 396, 123,  14,   0, 123,
 221,  13,   0,   0,  34,  35,  36,  34,  35,  36,
  16,   0,   0,  18,  28,   0,  33,  25,  26,  27,
  15,  31,   0,   0,  14,   0,   0,   0,  13,  34,
  35,  36, 126,   0,   0, 126, 127,  16, 126, 127,
  18,  28, 127,  33,  25,  26,  27,  15,  31,   0,
   0, 146,   0,   0,   0, 145,  34,  35,  36,   0,
   0,   0,   0,   0,  16,   0,   0,  18,  28,   0,
  33,  25,  26,  27,   0,  31,   0,   0,   0,   0,
   0,   0,   0,  34,  35,  36,  18,  28,   0,  33,
  25,  26,  27,   0,  31,   0,   0,   0,   0,   0,
   0,   0,  34,  35,  36,   0,  18,  28,   0,  33,
  25,  26,  27,   0,  31,   0,   0,   0,   0,   0,
 119,   0,  34,  35,  36,   0,   0,   0,   0,   0,
   0,   0, 344,  28,   0,  33,  25,  26,  27,   0,
  31,   0,   0,   0,   0,   0,  33,   0,  34,  35,
  36, 123,   0,   0,   0, 221,   0,   0,  33,  34,
  35,  36,   0, 345,   0,   0,   0,   0,   0,   0,
   0,  34,  35,  36,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,  33,   0,   0,  33,   0, 218,  33,
   0, 123,   0,   0, 218,   0,  34,  35,  36,  34,
  35,  36,  34,  35,  36 };
short yypact[]={

-127,-1000, 581,-214,-1000,-160,-1000,-1000,-208,  39,
  39, 927, 953,  40, 581, 581,  50, 953, 953,  43,
 117, 953,-1000,  48, 532,-1000,-1000,-1000,-1000, 581,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000, 421,-250,
-1000,-1000,-170,-1000,  40, 812, 861,-1000,-1000,-1000,
-1000,-1000,-248,-1000, 861, 953, 201,  43,-166, 834,
-139,-132, 888,  43,  43, 194, 953,  43, 194, 907,
 236, 228, 217,-1000, 213, 204,  39, 953, 953, 199,
-1000,  86,  20, 198,  14,-1000, 142, 141,-1000, 139,
-1000,-1000,-1000,-1000,-188,-188,-188,-219,-168, 812,
 812, 338, 812, 812, 268,  41,  75, 117,-1000, 265,
-1000,-1000,-1000,-1000,-1000,-1000,-232,-1000,-229,-1000,
1047,1047,-1000,-1000,-1000, 964, 343,-1000,  39,  82,
  66,-1000, 581,  43, 581,  14,-1000, 137,-1000, 581,
  45,  14, 136,-1000,-223,  40, 581,  29, 187,-1000,
 129,  64,  43,  16,-1000,-1000,-1000,-1000,-1000, 581,
-1000, 581, 793, 116, 114,-1000,-1000,-1000, 581, 744,
 581,-1000,-1000,-1000, 463,-1000,-256, 490,  39,-1000,
  39,  39, -16,-211,-1000, 483,-1000,-1000, 128,-233,
 127,-234,  34,-1000,-1000,-191,-160,-237,-1000,-160,
-242,-1000, 812, 812, 194,-1000,-160,-1000, 581,  26,
-1000, 581,-150, 266, 812, 812,-1000,-1000,-1000,-1000,
-1000, 259, 192, 184, 180,  67,-1000, 861,-1000,-1000,
-1000,-1000,-1000, 861,-141, 861,-1000, 888, 581,-166,
-139,-1000, 194, 581, 249, 236, 228,-1000,-1000,-1000,
-1000,-1000,-1000,-216, 183,-1000,-225,  40,-1000,-1000,
-1000, 463,-1000,-1000,-1000, 182,-1000, 182, 182,-1000,
 262, 315,   4, 812,-1000, 976,-168,  47,-168, 812,
 194,-1000,-1000, 812,-1000, 812, 226, 175,-1000,-1000,
-1000,-1000,-1000,-1000, 123,  38, 161,-1000,-1000,-211,
-1000, 243,-1000,-1000,-1000,-1000, 812,-1000,-1000, 812,
-1000,-1000,-1000, 581,  14, 135,-1000, -79,-1000,-1000,
-1000,-1000, 581, 744, 581,-166,-1000, 134,  39, 315,
 223,-1000, 169,-1000,-211,-1000,-169, -28,-1000,1044,
 510, 242,1041,  35,-1000,-1000, 809, 366,-177,1047,
-195, 174,-1000,-1000,-1000,-1000,-1000, 812, 581, 421,
-1000, 615,-1000, 222,-1000,-1000,-1000,-1000, 861,-160,
  18, 581,-1000,-1000, 861,-1000,-1000,-1000, 463,-1000,
 220,-1000, 627, -16, 194,-1000, 976,  52,  17, 432,
-197, 432,1047,1047,-1000, 194, 217, 976,-1000,-1000,
  47,-1000,-1000,  14,-1000,-1000, 214, 205,-1000, 167,
-1000,-1000,-1000,  37,-1000,-1000,-1000,-1000,-230,-1000,
-1000,-1000,-1000, 165,-1000,-213,-1000,-1000,-204, 432,
-1000,1044,-1000,-1000,-1000,-1000, 105,-1000,  25,-1000,
 -28,-177,-1000,-1000,-1000, 296, 581, 194, 812, 197,
 163,-1000,-1000,-1000,-1000, 194, 750,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-206,-1000,-1000, 812,-1000,-1000 };
short yypgo[]={

   0, 579,   5,  56, 575, 574,  28,  17, 571, 467,
  89, 570,  16, 169, 569, 252, 568,  23,  32, 172,
  25,  31, 567,  54, 566, 564,   1,   9,  33, 175,
 312,  41, 495,  15,   0, 560, 137, 652, 183,   7,
 557,  27,   4, 442,  12,   6,  14,  18,  11,  10,
  19,  13, 556, 552,  29,   2,   3, 551, 549, 541,
 105, 540, 536, 534, 533,  26, 448, 532, 220, 531,
 526, 146, 514, 505, 501, 164, 499, 197, 619, 487,
 180, 482, 479, 477, 917, 182, 444, 453, 451,  22,
 397, 363,  57, 346, 143, 344, 132, 311,  36 };
short yyr1[]={

   0,   1,   1,   1,   4,   4,   8,   8,   9,   9,
  10,  10,  10,   6,   6,   6,   6,   6,  11,  11,
  11,  16,  16,  17,  17,  14,  14,  15,  19,  19,
  19,  19,  20,  20,  20,  21,  21,  21,  21,  22,
  22,  18,  18,  18,  18,  24,  24,  24,  24,  25,
  25,  26,  26,  13,  13,  28,  28,  28,  29,  29,
  30,  30,  31,  31,  31,  32,  32,  32,  33,  33,
  12,  12,  12,  12,  35,  35,  35,  35,  36,  36,
  36,  36,  36,  36,  36,  36,  38,  38,  38,  40,
  40,  45,  45,  41,  41,  47,  47,  47,  47,  47,
  47,  47,  52,  52,  52,  49,  49,  53,  53,  55,
  55,  44,  42,  42,  42,  57,  57,  58,  58,  36,
  59,  59,  59,  60,  60,  36,  36,  36,  61,  61,
  62,  62,  63,  63,  64,  64,  46,  46,  43,  39,
  39,  39,  48,  48,  50,  50,  51,  51,  54,  54,
  65,  65,  65,  65,  65,  65,  65,  65,  65,  66,
  66,  67,  67,  68,  69,  69,  69,  69,  70,  70,
  37,  37,  72,  72,  73,  73,   3,   3,  74,  74,
  75,  56,  56,  23,  23,  76,  76,  76,  34,  34,
  34,  34,  27,  27,   2,   2,  77,  77,  71,  71,
  71,  79,  79,  78,  78,  78,  78,  78,  78,  78,
  81,  81,  80,  80,  80,  84,  84,  84,  84,  84,
  84,  84,  84,  84,  84,  84,  84,  84,  84,  84,
  84,  84,  84,  84,  86,  86,  82,  82,  88,  88,
  89,  90,  90,  90,  91,  91,  92,  83,  83,  93,
  93,  94,  94,  94,  94,  85,  85,  95,  95,  96,
  96,  87,  87,  87,  87,  87,  87,  87,  87,  97,
  97,  98,  98,  98,   5,   7,   7 };
short yyr2[]={

   0,   3,   2,   1,   3,   1,   2,   1,   7,   2,
   1,   1,   1,   1,   3,   2,   4,   6,   0,   3,
   4,   3,   1,   1,   2,   3,   1,   0,   3,   6,
   4,   2,   0,   4,   3,   0,   1,   1,   2,   3,
   1,   1,   1,   4,   4,   0,   1,   1,   2,   3,
   1,   1,   1,   3,   1,   3,   3,   3,   1,   0,
   3,   1,   1,   1,   1,   1,   1,   3,   1,   3,
   0,   1,   1,   2,   3,   3,   1,   1,   4,   6,
   5,   7,   2,   4,   5,   7,   2,   1,   1,   3,
   1,   3,   1,   3,   1,   4,   3,   3,   1,   1,
   4,   1,   3,   3,   2,   2,   1,   3,   1,   3,
   4,   2,   0,   2,   4,   0,   1,   3,   1,   4,
   3,   1,   1,   2,   1,   3,   3,   4,   3,   1,
   3,   1,   0,   1,   3,   1,   3,   1,   1,   1,
   3,   1,   1,   1,   2,   1,   2,   1,   1,   1,
   1,   2,   3,   3,   3,   3,   3,   2,   1,   2,
   1,   3,   3,   3,   0,   1,   1,   2,   3,   1,
   3,   2,   2,   1,   2,   1,   2,   0,   2,   1,
   4,   3,   1,   1,   3,   1,   3,   3,   1,   1,
   1,   1,   1,   3,   1,   1,   3,   1,   1,   3,
   1,   3,   5,   2,   4,   4,   6,   6,   4,   1,
   2,   1,   2,   2,   1,   1,   3,   2,   1,   1,
   4,   4,   2,   1,   1,   1,   1,   3,   3,   3,
   4,   4,   4,   3,   3,   3,   1,   2,   3,   1,
   3,   1,   2,   1,   2,   1,   4,   2,   1,   3,
   1,   3,   2,   2,   1,   0,   1,   3,   1,   1,
   3,   0,   1,   1,   3,   3,   4,   2,   5,   3,
   1,   3,   1,   2,   1,   1,   1 };
short yychk[]={

-1000,  -1, 257, 258, 256,  -2, -77, 256, -71, -78,
 -79,  45,  92, 267, 263, 259, 276, -80, 279, -84,
 -23, 126,  95, -27,  40, 283, 284, 285, 280,  91,
 -76, 287, -34, 282, 295, 296, 297,  -4,  -5,  -8,
 256,  -9, 293,  -3, 266, 288, -31, -32, -33,  45,
 281,  33,  96, 286, -31, -80, -81, -84, -68, 123,
  -2,  -2, 123, -84, -84, 123,  64, -84, 123,  45,
 281,  33, 286,  41,  -2, -86, -78, -32, -33, -66,
  44, -87,  -2, -86,  -6, -12, -13, -14,  59, -35,
 -28, -19, -36, -37, 270, 271, 269, 294, 262, 261,
 273, 272, 277, 278, 274, -56, -71, -23,  -9, -10,
 256, 287, 282, 285, -68, -46, -43, -39, -48, 256,
 -50, -51, -65, 287, -34,  40,  91,  95, -78, -34,
 287, -78, 291, -84, 268, -69,  59, -70, -37, 264,
 260, -83, -93, -94, -77, 267, 263, -85, -95, -96,
 -23,  40, -84, -85,  41,  41,  41,  41,  41,  44,
  41,  44, -31, -84, -84,  44,  41,  93,  44, 124,
 289,  -7, 125, 256,  59, -15,  59,  59, -29, 283,
 -29, -29, -10, 296, 256, -38, 287, 256, -39, -43,
 -39, -43, -59, -60, 256, -23, -61, -43, -39, -62,
 -43, -39,  40, 288,  44, -72, -73, 256,  61, -74,
 -75, 124, -11,  40, 292, 291, -54, -65, 287, -54,
  41, 291, -39, -66, -67, -39,  93, -31,  96,  96,
  -2,  -2,  -7,  59,  -2, 123,  -7,  59, 290, -68,
  -2, 125,  44,  61,  45, 281,  33, 125,  -2,  -2,
  41,  41,  41,  -2, -97, -98,  -2, 267,  -2, -12,
 -28, -15, -19, -36, -37, -30, -31, -30, -30, -20,
 295,  40, -10,  61, -34,  61, 292,  61, 292, 288,
  44, 285,  -3, 292,  -3, 292, -63, -64, -39, -46,
 -23,  -3,  -2, -75,  -2, 266, -16, -17, -18, 293,
 -23, 287, -39, -39,  41,  41,  44,  41,  41,  44,
  93, -78, -37, 265, -82, -88, -89, -71, -94,  -2,
 -96,  -2, 289,  44, 290, -68, -12, -13,  44,  40,
 -21,  44, -22, -18, 297, -20, -39, -41, -47,  33,
 -50, -51, -52, -27, 256, 287,  40, -38, -44, -27,
  40, -38, -46, -60, -39, -39,  41,  44,  61, 123,
  41,  44, -10,  40, -39, -39,  -2,  -7,  59, -90,
 -91, 291, 256, -92, 124,  -2, -98,  -2,  59, -31,
 -21,  41,  44, -10, 268, -42, 124, 275, -48, -33,
  96, -33,  33,  33, -54, 123, 286,  61, -42, -54,
  61, -39,  -2,  -6,  41, -17, 289, -24,  44, -25,
 -26, -23, -27,  40, -89,  -3, -92,  -2, -71, -12,
  41, -18, -20, -40, -45, -23, -47, 287,  40, -33,
 -49,  33, -48, -49, -54, -54, -53, -55, -56, -23,
 -41, -44,  -7,  41,  41,  44, 291,  44, 288, -57,
 -58, 287, -49, -48, 125,  44, 288, -42, -42, -26,
  -2, -45, -46,  41,  44, -55, -39,  33, 287, -39 };
short yydef[]={

   0,  -2,   0,   0,   3, 177, 194, 195, 197, 198,
 200,   0,   0,   0,   0,   0,   0, 209,   0, 214,
 215,   0, 218, 219,   0, 223, 224, 225, 226, 261,
 183, 192, 185, 188, 189, 190, 191,   2,  70,   5,
 274,   7,   0,   1,   0,   0,   0,  62,  63,  64,
  65,  66,   0,  68,   0, 203,   0, 211,   0, 164,
   0,   0,   0, 212, 213, 255,   0, 217, 255,   0,
  65,  66,  68, 222,   0,   0, 198,   0,   0,   0,
 160,   0, 262, 263,   0,  13,   0,  27,  71,  72,
  54,  26,  76,  77,  59,  59,  59,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,  -2,   6,  18,
   9,  10,  11,  12, 176, 196,   0,  -2, 139, 141,
 142, 143, 145, 147, 150,   0,   0, 158, 199,   0,
   0, 201,   0, 210,   0,   0, 165, 166, 169,   0,
   0,   0, 248, 250, 254,   0,   0,   0, 256, 258,
 259,   0, 216,   0, 184, 186, 187, 193, 227,   0,
 228,   0,   0,   0,   0, 159, 233, 229,   0,   0,
 267,   4, 275, 276,  70,  15,  27,  73,   0,  58,
   0,   0,  32,   0,  31,   0,  87,  88,  -2,   0,
 138,   0,   0, 121, 122, 124, 177,   0,  -2, 177,
   0,  -2, 132,   0,   0, 171, 177, 173,   0, 175,
 179,   0,   0,   0,   0,   0, 144, 148, 149, 146,
 151,   0,   0,   0,   0,   0, 157,   0,  67,  69,
 204, 205, 163, 167,   0,   0, 208, 247,   0, 252,
 253, 221,   0,   0,   0,   0,   0, 220, 235, 234,
 230, 231, 232, 235, 264, 270, 272,   0, 265,  14,
  53,  70,  25,  74,  75,  55,  61,  56,  57,  28,
   0,  35,  32,   0,  86,   0,   0,   0,   0,   0,
   0, 123, 125,   0, 126,   0,   0, 133, 135, 170,
 181, 172, 174, 178,   0,   0,   0,  22,  23,   0,
  41,  42, 136, 140, 152, 153,   0, 154, 155,   0,
 156, 202, 168,   0,   0, 236, 239,   0, 249, 251,
 257, 260, 266,   0,   0, 273,  16,   0,   0,  35,
   0,  36,  37,  40,   0,  30,  78, 112,  94,   0,
   0,  98,  99,   0, 101,  -2,   0,  83, 112,   0,
   0,   0, 119, 120, 128, 130, 127,   0,   0,  70,
  19,   0,  24,  45, 162, 161, 206, 207, 237, 177,
 241,   0, 243, 245,   0, 268, 269, 271,  70,  60,
   0,  34,  38,  32,   0,  80,   0,   0,   0,   0,
   0,   0,   0,   0, 104,   0,   0,   0,  84, 111,
   0, 134, 180,   0,  20,  21,   0,   0,  46,  47,
  50,  51,  52,   0, 238, 240, 244, 242,   0,  17,
  33,  39,  29,  79,  90,  92,  93, 113, 115,   0,
  96,   0, 106,  97, 102, 103,   0, 108,   0, 182,
 112, 112,   8,  43,  44,  48,   0,   0,   0,   0,
 116, 118,  95, 105, 100,   0,   0,  81,  85,  49,
 246,  89,  91, 114,   0, 107, 109,   0, 117, 110 };
#define YYFLAG   -1000
#define YYERROR  goto yyerrlab
#define YYACCEPT return(0)
#define YYABORT  return(1)

/*      parser for yacc output  */

#ifdef YYDEBUG
int yydebug     = 0;     /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar      = -1;    /* current input token number */
int yynerrs     = 0;     /* number of errors */
short yyerrflag = 0;     /* error recovery flag */

int yyparse() {
    short yys[YYMAXDEPTH];
    short yyj, yym;
    register YYSTYPE *yypvt;
    register short yystate, *yyps, yyn;
    register YYSTYPE *yypv;
    register short *yyxi;

    yystate   = 0;
    yychar    = -1;
    yynerrs   = 0;
    yyerrflag = 0;
    yyps      = &yys[-1];
    yypv      = &yyv[-1];

yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
    if (yydebug)
        printf("state %d, char 0%o\n", yystate, yychar);
#endif
    if(++yyps>&yys[YYMAXDEPTH]) {
        yyerror("yacc stack overflow");
        return(1);
    }
    *yyps = yystate;
    ++yypv;
#ifdef UNION
    yyunion(yypv, &yyval);
#else
    *yypv = yyval;
#endif

yynewstate:

    yyn = yypact[yystate];

    if (yyn<=YYFLAG)
        goto yydefault; /* simple state */

    if (yychar<0)
        if ((yychar=yylex())<0)
            yychar=0;
    if ((yyn+=yychar)<0 || yyn>=YYLAST)
        goto yydefault;

    if (yychk[yyn=yyact[yyn]]==yychar) {
        /* valid shift */
        yychar = -1;
#ifdef UNION
        yyunion(&yyval, &yylval);
#else
        yyval = yylval;
#endif
        yystate = yyn;
        if (yyerrflag>0)
            --yyerrflag;
        goto yystack;

    }

yydefault:

    /* default state action */

    if ((yyn=yydef[yystate])== -2) {
        if (yychar<0)
            if ((yychar=yylex())<0)
                yychar = 0;
        /* look through exception table */

        for (yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2)
            ; /* VOID */

        for (yyxi+=2; *yyxi >= 0; yyxi+=2) {
            if (*yyxi==yychar)
                break;
        }
        if ((yyn=yyxi[1])<0)
            return(0);   /* accept */
    }

    if (yyn==0) {
        /* error */
        /* error ... attempt to resume parsing */

        switch (yyerrflag) {
            case 0: /* brand new error */
                    yyerror( "syntax error" );

yyerrlab:           ++yynerrs;

            case 1:
            case 2: /* incompletely recovered error ... try again */

                    yyerrflag = 3;

                    /* find a state where "error" is a legal shift action */

                    while (yyps>=yys) {
                        yyn = yypact[*yyps] + YYERRCODE;
                        if (yyn>=0 && yyn<YYLAST
                                   && yychk[yyact[yyn]]==YYERRCODE) {
                            yystate = yyact[yyn];
                            /* simulate a shift of "error" */
                            goto yystack;
                        }
                        yyn = yypact[*yyps];

                        /* the current yyps has no shift on "error",
                           pop stack */

#ifdef YYDEBUG
                        if (yydebug)
                            printf("error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1]);
#endif

                        --yyps;
                        --yypv;
                    }

                    /* there is no state on the stack with an error shift
                       ... abort */

yyabort:            return(1);


            case 3: /* no shift yet; clobber input char */
#ifdef YYDEBUG
                    if (yydebug)
                        printf("error recovery discards char %d\n", yychar);
#endif

                    if (yychar==0)
                        goto yyabort; /* don't discard EOF, quit */
                    yychar = -1;
                    goto yynewstate;   /* try again in the same state */
        }
    }

    /* reduction by production yyn */

#ifdef YYDEBUG
    if (yydebug)
        printf("reduce %d\n",yyn);
#endif
    yyps -= yyr2[yyn];
    yypvt = yypv;
    yypv -= yyr2[yyn];
#ifdef UNION
    yyunion(&yyval, &yypv[1]);
#else
    yyval = yypv[1];
#endif
    yym=yyn;
    /* consult goto table to find next state */
    yyn = yyr1[yyn];
    yyj = yypgo[yyn] + *yyps + 1;
    if (yyj>=YYLAST || yychk[yystate=yyact[yyj]]!= -yyn)
        yystate = yyact[yypgo[yyn]];
    switch(yym) {
        
case 1:
# line 83
{inputExpr = letrec(yypvt[-0],yypvt[-1]); sp-=2;} break;
case 2:
# line 84
{valDefns  = yypvt[-0];            sp-=1;} break;
case 3:
# line 85
{syntaxError("input");} break;
case 4:
# line 94
{yyval = gc2(yypvt[-1]);} break;
case 5:
# line 95
{yyval = yypvt[-0];} break;
case 6:
# line 97
{yyval = gc2(appendOnto(yypvt[-0],yypvt[-1]));} break;
case 7:
# line 98
{yyval = yypvt[-0];} break;
case 8:
# line 101
{yyval = gc7(yypvt[-1]);} break;
case 9:
# line 102
{syntaxError("module definition");} break;
case 10:
# line 104
{yyval = yypvt[-0];} break;
case 11:
# line 105
{yyval = yypvt[-0];} break;
case 12:
# line 106
{yyval = yypvt[-0];} break;
case 13:
# line 108
{yyval = yypvt[-0];} break;
case 14:
# line 109
{yyval = gc3(yypvt[-0]);} break;
case 15:
# line 110
{yyval = gc2(NIL);} break;
case 16:
# line 111
{yyval = gc4(yypvt[-0]);} break;
case 17:
# line 113
{yyval = gc6(yypvt[-0]);} break;
case 18:
# line 118
{yyval = gc0(NIL);} break;
case 19:
# line 119
{yyval = gc3(NIL);} break;
case 20:
# line 120
{yyval = gc4(NIL);} break;
case 21:
# line 122
{yyval = gc3(NIL);} break;
case 22:
# line 123
{yyval = yypvt[-0];} break;
case 23:
# line 125
{yyval = yypvt[-0];} break;
case 24:
# line 126
{yyval = gc2(NIL);} break;
case 25:
# line 131
{imps = cons(yypvt[-0],imps); yyval=gc3(NIL);} break;
case 26:
# line 132
{imps = singleton(yypvt[-0]); yyval=gc1(NIL);} break;
case 27:
# line 134
{if (chase(imps)) {
					     clearStack();
					     onto(imps);
                                             done();
					     closeAnyInput();
					     return 0;
					 }
					 yyval = gc0(NIL);
					} break;
case 28:
# line 144
{yyval = gc3(yypvt[-1]);} break;
case 29:
# line 146
{yyval = gc6(yypvt[-3]);} break;
case 30:
# line 148
{yyval = gc4(yypvt[-1]);} break;
case 31:
# line 149
{syntaxError("import declaration");} break;
case 32:
# line 151
{yyval = gc0(NIL);} break;
case 33:
# line 152
{yyval = gc4(NIL);} break;
case 34:
# line 153
{yyval = gc3(NIL);} break;
case 35:
# line 155
{yyval = gc0(NIL);} break;
case 36:
# line 156
{yyval = gc1(NIL);} break;
case 37:
# line 157
{yyval = yypvt[-0];} break;
case 38:
# line 158
{yyval = gc2(yypvt[-1]);} break;
case 39:
# line 160
{yyval = gc3(NIL);} break;
case 40:
# line 161
{yyval = yypvt[-0];} break;
case 41:
# line 163
{yyval = yypvt[-0];} break;
case 42:
# line 164
{yyval = yypvt[-0];} break;
case 43:
# line 165
{yyval = gc4(NIL);} break;
case 44:
# line 166
{yyval = gc4(NIL);} break;
case 45:
# line 168
{yyval = gc0(NIL);} break;
case 46:
# line 169
{yyval = gc1(NIL);} break;
case 47:
# line 170
{yyval = yypvt[-0];} break;
case 48:
# line 171
{yyval = gc2(yypvt[-1]);} break;
case 49:
# line 173
{yyval = gc3(NIL);} break;
case 50:
# line 174
{yyval = gc1(NIL);} break;
case 51:
# line 176
{yyval = yypvt[-0];} break;
case 52:
# line 177
{yyval = yypvt[-0];} break;
case 53:
# line 182
{yyval = gc2(NIL);} break;
case 54:
# line 183
{yyval = gc0(NIL);} break;
case 55:
# line 185
{fixDefn(LEFT_ASS,yypvt[-2],yypvt[-1],yypvt[-0]); sp-=3;} break;
case 56:
# line 186
{fixDefn(RIGHT_ASS,yypvt[-2],yypvt[-1],yypvt[-0]);sp-=3;} break;
case 57:
# line 187
{fixDefn(NON_ASS,yypvt[-2],yypvt[-1],yypvt[-0]);  sp-=3;} break;
case 58:
# line 189
{yyval = gc1(checkPrec(yypvt[-0]));} break;
case 59:
# line 190
{yyval = gc0(mkInt(DEF_PREC));} break;
case 60:
# line 192
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 61:
# line 193
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 62:
# line 195
{yyval = yypvt[-0];} break;
case 63:
# line 196
{yyval = yypvt[-0];} break;
case 64:
# line 197
{yyval = gc1(varMinus);} break;
case 65:
# line 199
{yyval = yypvt[-0];} break;
case 66:
# line 200
{yyval = gc1(varBang);} break;
case 67:
# line 201
{yyval = gc3(yypvt[-1]);} break;
case 68:
# line 203
{yyval = yypvt[-0];} break;
case 69:
# line 204
{yyval = gc3(yypvt[-1]);} break;
case 70:
# line 209
{yyval = gc0(NIL);} break;
case 71:
# line 210
{yyval = gc1(NIL);} break;
case 72:
# line 211
{yyval = yypvt[-0];} break;
case 73:
# line 212
{yyval = gc2(yypvt[-1]);} break;
case 74:
# line 214
{yyval = gc2(yypvt[-2]);} break;
case 75:
# line 215
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 76:
# line 216
{yyval = gc0(NIL);} break;
case 77:
# line 217
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 78:
# line 222
{defTycon(4,yypvt[-1],yypvt[-2],yypvt[-0],SYNONYM);} break;
case 79:
# line 224
{defTycon(6,yypvt[-3],yypvt[-4],
						    ap(yypvt[-2],yypvt[-0]),RESTRICTSYN);} break;
case 80:
# line 227
{defTycon(5,yypvt[-2],checkTyLhs(yypvt[-3]),
						    ap(rev(yypvt[-1]),yypvt[-0]),DATATYPE);} break;
case 81:
# line 230
{defTycon(7,yypvt[-2],yypvt[-3],
						  ap(ap(QUAL,pair(yypvt[-5],rev(yypvt[-1]))),
						     yypvt[-0]),DATATYPE);} break;
case 82:
# line 233
{defTycon(2,yypvt[-1],checkTyLhs(yypvt[-0]),
						    ap(NIL,NIL),DATATYPE);} break;
case 83:
# line 235
{defTycon(4,yypvt[-3],yypvt[-0],
						  ap(ap(QUAL,pair(yypvt[-2],NIL)),
						     NIL),DATATYPE);} break;
case 84:
# line 239
{defTycon(5,yypvt[-2],checkTyLhs(yypvt[-3]),
						    ap(yypvt[-1],yypvt[-0]),NEWTYPE);} break;
case 85:
# line 242
{defTycon(7,yypvt[-2],yypvt[-3],
						  ap(ap(QUAL,pair(yypvt[-5],yypvt[-1])),
						     yypvt[-0]),NEWTYPE);} break;
case 86:
# line 246
{yyval = gc2(ap(yypvt[-1],yypvt[-0]));} break;
case 87:
# line 247
{yyval = yypvt[-0];} break;
case 88:
# line 248
{syntaxError("type defn lhs");} break;
case 89:
# line 250
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 90:
# line 251
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 91:
# line 253
{yyval = gc3(sigdecl(yypvt[-1],singleton(yypvt[-2]),
							     yypvt[-0]));} break;
case 92:
# line 255
{yyval = yypvt[-0];} break;
case 93:
# line 257
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 94:
# line 258
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 95:
# line 260
{yyval = gc4(ap(ap(yypvt[-1],bang(yypvt[-2])),yypvt[-0]));} break;
case 96:
# line 261
{yyval = gc3(ap(ap(yypvt[-1],yypvt[-2]),yypvt[-0]));} break;
case 97:
# line 262
{yyval = gc3(ap(ap(yypvt[-1],yypvt[-2]),yypvt[-0]));} break;
case 98:
# line 263
{yyval = yypvt[-0];} break;
case 99:
# line 264
{yyval = yypvt[-0];} break;
case 100:
# line 265
{yyval = gc4(ap(LABC,pair(yypvt[-3],rev(yypvt[-1]))));} break;
case 101:
# line 266
{syntaxError("data type definition");} break;
case 102:
# line 268
{yyval = gc3(ap(yypvt[-2],bang(yypvt[-0])));} break;
case 103:
# line 269
{yyval = gc3(ap(yypvt[-2],bang(yypvt[-0])));} break;
case 104:
# line 270
{yyval = gc2(ap(yypvt[-1],yypvt[-0]));} break;
case 105:
# line 272
{yyval = gc2(bang(yypvt[-0]));} break;
case 106:
# line 273
{yyval = yypvt[-0];} break;
case 107:
# line 275
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 108:
# line 276
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 109:
# line 278
{yyval = gc3(pair(rev(yypvt[-2]),yypvt[-0]));} break;
case 110:
# line 279
{yyval = gc4(pair(rev(yypvt[-3]),bang(yypvt[-0])));} break;
case 111:
# line 281
{yyval = gc2(singleton(ap(yypvt[-1],yypvt[-0])));} break;
case 112:
# line 283
{yyval = gc0(NIL);} break;
case 113:
# line 284
{yyval = gc2(singleton(yypvt[-0]));} break;
case 114:
# line 285
{yyval = gc4(yypvt[-1]);} break;
case 115:
# line 287
{yyval = gc0(NIL);} break;
case 116:
# line 288
{yyval = gc1(rev(yypvt[-0]));} break;
case 117:
# line 290
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 118:
# line 291
{yyval = gc1(singleton(yypvt[-0]));} break;
case 119:
# line 296
{primDefn(yypvt[-3],yypvt[-2],yypvt[-0]); sp-=4;} break;
case 120:
# line 298
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 121:
# line 299
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 122:
# line 300
{syntaxError("primitive defn");} break;
case 123:
# line 302
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 124:
# line 303
{yyval = yypvt[-0];} break;
case 125:
# line 308
{classDefn(intOf(yypvt[-2]),yypvt[-1],yypvt[-0]); sp-=3;} break;
case 126:
# line 309
{instDefn(intOf(yypvt[-2]),yypvt[-1],yypvt[-0]);  sp-=3;} break;
case 127:
# line 310
{defaultDefn(intOf(yypvt[-3]),yypvt[-1]);  sp-=4;} break;
case 128:
# line 312
{yyval = gc3(pair(yypvt[-2],checkClass(yypvt[-0])));} break;
case 129:
# line 313
{yyval = gc1(pair(NIL,checkClass(yypvt[-0])));} break;
case 130:
# line 315
{yyval = gc3(pair(yypvt[-2],checkInst(yypvt[-0])));} break;
case 131:
# line 316
{yyval = gc1(pair(NIL,checkInst(yypvt[-0])));} break;
case 132:
# line 318
{yyval = gc0(NIL);} break;
case 133:
# line 319
{yyval = gc1(rev(yypvt[-0]));} break;
case 134:
# line 321
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 135:
# line 322
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 136:
# line 333
{yyval = gc3(ap(QUAL,pair(yypvt[-2],yypvt[-0])));} break;
case 137:
# line 334
{yyval = yypvt[-0];} break;
case 138:
# line 336
{yyval = gc1(checkContext(yypvt[-0]));} break;
case 139:
# line 338
{yyval = yypvt[-0];} break;
case 140:
# line 339
{yyval = gc3(ap(ap(typeArrow,yypvt[-2]),yypvt[-0]));} break;
case 141:
# line 340
{syntaxError("type expression");} break;
case 142:
# line 342
{yyval = yypvt[-0];} break;
case 143:
# line 343
{yyval = yypvt[-0];} break;
case 144:
# line 345
{yyval = gc2(ap(yypvt[-1],yypvt[-0]));} break;
case 145:
# line 346
{yyval = yypvt[-0];} break;
case 146:
# line 348
{yyval = gc2(ap(yypvt[-1],yypvt[-0]));} break;
case 147:
# line 349
{yyval = yypvt[-0];} break;
case 148:
# line 351
{yyval = yypvt[-0];} break;
case 149:
# line 352
{yyval = yypvt[-0];} break;
case 150:
# line 354
{yyval = yypvt[-0];} break;
case 151:
# line 355
{yyval = gc2(typeUnit);} break;
case 152:
# line 356
{yyval = gc3(typeArrow);} break;
case 153:
# line 357
{yyval = gc3(yypvt[-1]);} break;
case 154:
# line 358
{yyval = gc3(yypvt[-1]);} break;
case 155:
# line 359
{yyval = gc3(buildTuple(yypvt[-1]));} break;
case 156:
# line 360
{yyval = gc3(ap(typeList,yypvt[-1]));} break;
case 157:
# line 361
{yyval = gc2(typeList);} break;
case 158:
# line 362
{yyval = gc1(inventVar());} break;
case 159:
# line 364
{yyval = gc2(mkTuple(tupleOf(yypvt[-1])+1));} break;
case 160:
# line 365
{yyval = gc1(mkTuple(2));} break;
case 161:
# line 367
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 162:
# line 368
{yyval = gc3(cons(yypvt[-0],cons(yypvt[-2],NIL)));} break;
case 163:
# line 373
{yyval = gc3(yypvt[-1]);} break;
case 164:
# line 375
{yyval = gc0(NIL);} break;
case 165:
# line 376
{yyval = gc1(NIL);} break;
case 166:
# line 377
{yyval = yypvt[-0];} break;
case 167:
# line 378
{yyval = gc2(yypvt[-1]);} break;
case 168:
# line 380
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 169:
# line 381
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 170:
# line 383
{yyval = gc3(sigdecl(yypvt[-1],yypvt[-2],yypvt[-0]));} break;
case 171:
# line 384
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 172:
# line 386
{yyval = gc2(letrec(yypvt[-0],yypvt[-1]));} break;
case 173:
# line 387
{syntaxError("declaration");} break;
case 174:
# line 389
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 175:
# line 390
{yyval = gc1(grded(rev(yypvt[-0])));} break;
case 176:
# line 392
{yyval = gc2(yypvt[-0]);} break;
case 177:
# line 393
{yyval = gc0(NIL);} break;
case 178:
# line 395
{yyval = gc2(cons(yypvt[-0],yypvt[-1]));} break;
case 179:
# line 396
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 180:
# line 398
{yyval = gc4(pair(yypvt[-1],pair(yypvt[-2],yypvt[-0])));} break;
case 181:
# line 400
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 182:
# line 401
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 183:
# line 403
{yyval = yypvt[-0];} break;
case 184:
# line 404
{yyval = gc3(varMinus);} break;
case 185:
# line 406
{yyval = yypvt[-0];} break;
case 186:
# line 407
{yyval = gc3(yypvt[-1]);} break;
case 187:
# line 408
{yyval = gc3(varBang);} break;
case 188:
# line 410
{yyval = yypvt[-0];} break;
case 189:
# line 411
{yyval = gc1(varHiding);} break;
case 190:
# line 412
{yyval = gc1(varQualified);} break;
case 191:
# line 413
{yyval = gc1(varAsMod);} break;
case 192:
# line 415
{yyval = yypvt[-0];} break;
case 193:
# line 416
{yyval = gc3(yypvt[-1]);} break;
case 194:
# line 421
{yyval = yypvt[-0];} break;
case 195:
# line 422
{syntaxError("expression");} break;
case 196:
# line 424
{yyval = gc3(ap(ESIGN,pair(yypvt[-2],yypvt[-0])));} break;
case 197:
# line 425
{yyval = yypvt[-0];} break;
case 198:
# line 427
{yyval = yypvt[-0];} break;
case 199:
# line 428
{yyval = gc3(ap(ap(yypvt[-1],yypvt[-2]),yypvt[-0]));} break;
case 200:
# line 429
{yyval = gc1(tidyInfix(yypvt[-0]));} break;
case 201:
# line 431
{yyval = gc3(ap(ap(yypvt[-1],yypvt[-2]),yypvt[-0]));} break;
case 202:
# line 432
{yyval = gc5(ap(ap(yypvt[-1],
							ap(ap(yypvt[-3],singleton(yypvt[-4])),
							   yypvt[-2])),yypvt[-0]));} break;
case 203:
# line 436
{if (isInt(yypvt[-0]))
					     yyval = gc2(mkInt(-intOf(yypvt[-0])));
					 else
					     yyval = gc2(ap(nameNegate,yypvt[-0]));
					} break;
case 204:
# line 441
{yyval = gc4(ap(LAMBDA,
						     pair(rev(yypvt[-2]),
							  pair(yypvt[-1],yypvt[-0]))));} break;
case 205:
# line 444
{yyval = gc4(letrec(yypvt[-2],yypvt[-0]));} break;
case 206:
# line 445
{yyval = gc6(ap(COND,triple(yypvt[-4],yypvt[-2],yypvt[-0])));} break;
case 207:
# line 446
{yyval = gc6(ap(CASE,pair(yypvt[-4],rev(yypvt[-1]))));} break;
case 208:
# line 447
{yyval = gc4(ap(DOCOMP,checkDo(yypvt[-1])));} break;
case 209:
# line 448
{yyval = yypvt[-0];} break;
case 210:
# line 450
{yyval = gc2(cons(yypvt[-0],yypvt[-1]));} break;
case 211:
# line 451
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 212:
# line 453
{yyval = gc2(ap(yypvt[-1],yypvt[-0]));} break;
case 213:
# line 454
{yyval = gc2(ap(RUNST,yypvt[-0]));} break;
case 214:
# line 455
{yyval = yypvt[-0];} break;
case 215:
# line 457
{yyval = yypvt[-0];} break;
case 216:
# line 458
{yyval = gc3(ap(ASPAT,pair(yypvt[-2],yypvt[-0])));} break;
case 217:
# line 459
{yyval = gc2(ap(LAZYPAT,yypvt[-0]));} break;
case 218:
# line 460
{yyval = gc1(WILDCARD);} break;
case 219:
# line 461
{yyval = yypvt[-0];} break;
case 220:
# line 462
{yyval = gc4(ap(CONFLDS,pair(yypvt[-3],yypvt[-1])));} break;
case 221:
# line 463
{yyval = gc4(ap(UPDFLDS,
						     triple(yypvt[-3],NIL,yypvt[-1])));} break;
case 222:
# line 465
{yyval = gc2(nameUnit);} break;
case 223:
# line 466
{yyval = yypvt[-0];} break;
case 224:
# line 467
{yyval = yypvt[-0];} break;
case 225:
# line 468
{yyval = yypvt[-0];} break;
case 226:
# line 469
{yyval = yypvt[-0];} break;
case 227:
# line 470
{yyval = gc3(yypvt[-1]);} break;
case 228:
# line 471
{yyval = gc3(buildTuple(yypvt[-1]));} break;
case 229:
# line 472
{yyval = gc3(yypvt[-1]);} break;
case 230:
# line 473
{yyval = gc4(ap(yypvt[-1],yypvt[-2]));} break;
case 231:
# line 474
{yyval = gc4(ap(ap(nameFlip,yypvt[-2]),yypvt[-1]));} break;
case 232:
# line 475
{yyval = gc4(ap(ap(nameFlip,yypvt[-2]),yypvt[-1]));} break;
case 233:
# line 476
{yyval = gc3(yypvt[-1]);} break;
case 234:
# line 478
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 235:
# line 479
{yyval = gc3(cons(yypvt[-0],cons(yypvt[-2],NIL)));} break;
case 236:
# line 481
{yyval = yypvt[-0];} break;
case 237:
# line 482
{yyval = gc2(yypvt[-1]);} break;
case 238:
# line 484
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 239:
# line 485
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 240:
# line 487
{yyval = gc3(pair(yypvt[-2],letrec(yypvt[-0],yypvt[-1])));} break;
case 241:
# line 489
{yyval = gc1(grded(rev(yypvt[-0])));} break;
case 242:
# line 490
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 243:
# line 491
{syntaxError("case expression");} break;
case 244:
# line 493
{yyval = gc2(cons(yypvt[-0],yypvt[-1]));} break;
case 245:
# line 494
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 246:
# line 496
{yyval = gc4(pair(yypvt[-1],pair(yypvt[-2],yypvt[-0])));} break;
case 247:
# line 498
{yyval = gc2(yypvt[-1]);} break;
case 248:
# line 499
{yyval = yypvt[-0];} break;
case 249:
# line 501
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 250:
# line 502
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 251:
# line 504
{yyval = gc3(ap(FROMQUAL,pair(yypvt[-2],yypvt[-0])));} break;
case 252:
# line 505
{yyval = gc2(ap(QWHERE,yypvt[-0]));} break;
case 253:
# line 506
{yyval = gc2(ap(BOOLQUAL,yypvt[-0]));} break;
case 254:
# line 507
{yyval = gc1(ap(DOQUAL,yypvt[-0]));} break;
case 255:
# line 509
{yyval = gc0(NIL);} break;
case 256:
# line 510
{yyval = gc1(rev(yypvt[-0]));} break;
case 257:
# line 512
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 258:
# line 513
{yyval = gc1(singleton(yypvt[-0]));} break;
case 259:
# line 515
{yyval = yypvt[-0];} break;
case 260:
# line 516
{yyval = gc3(pair(yypvt[-2],yypvt[-0]));} break;
case 261:
# line 521
{yyval = gc0(nameNil);} break;
case 262:
# line 522
{yyval = gc1(ap(FINLIST,cons(yypvt[-0],NIL)));} break;
case 263:
# line 523
{yyval = gc1(ap(FINLIST,rev(yypvt[-0])));} break;
case 264:
# line 524
{yyval = gc3(ap(COMP,pair(yypvt[-2],rev(yypvt[-0]))));} break;
case 265:
# line 525
{yyval = gc3(ap(ap(nameFromTo,yypvt[-2]),yypvt[-0]));} break;
case 266:
# line 526
{yyval = gc4(ap(ap(nameFromThen,yypvt[-3]),yypvt[-1]));} break;
case 267:
# line 527
{yyval = gc2(ap(nameFrom,yypvt[-1]));} break;
case 268:
# line 528
{yyval = gc5(ap(ap(ap(nameFromThenTo,
							       yypvt[-4]),yypvt[-2]),yypvt[-0]));} break;
case 269:
# line 531
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 270:
# line 532
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 271:
# line 534
{yyval = gc3(ap(FROMQUAL,pair(yypvt[-2],yypvt[-0])));} break;
case 272:
# line 535
{yyval = gc1(ap(BOOLQUAL,yypvt[-0]));} break;
case 273:
# line 536
{yyval = gc2(ap(QWHERE,yypvt[-0]));} break;
case 274:
# line 541
{yyerrok; goOffside(startColumn);} break;
case 275:
# line 544
{yyval = yypvt[-0];} break;
case 276:
# line 545
{yyerrok;
					 if (canUnOffside()) {
					     unOffside();
					     /* insert extra token on stack*/
					     push(NIL);
					     pushed(0) = pushed(1);
					     pushed(1) = mkInt(column);
					 }
					 else
					     syntaxError("definition");
					} break;/* End of actions */
    }
    goto yystack;  /* stack new state and value */

}
