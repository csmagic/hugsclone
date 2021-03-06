/* --------------------------------------------------------------------------
 * input.c:     Copyright (c) Mark P Jones 1991-1996.   All rights reserved.
 *              See NOTICE for details and conditions of use etc...
 *              Hugs version 1.3, August 1996
 *
 * Input functions, lexical analysis parsing etc...
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "command.h"
#include "errors.h"
#include <ctype.h>

/* --------------------------------------------------------------------------
 * Global data:
 * ------------------------------------------------------------------------*/

List tyconDefns	     = NIL;		/* type constructor definitions	   */
List typeInDefns     = NIL;		/* type synonym restrictions 	   */
List valDefns	     = NIL;		/* value definitions in script	   */
List opDefns	     = NIL;		/* operator defns in script	   */
List classDefns      = NIL;		/* class defns in script 	   */
List instDefns       = NIL;		/* instance defns in script	   */
List selDefns	     = NIL;		/* list of selector lists	   */
List overDefns	     = NIL;		/* overloaded implementation names */
List primDefns	     = NIL;		/* primitive definitions	   */
List defaultDefns    = NIL;		/* default definitions (if any)	   */
Int  defaultLine     = 0;		/* line in which default defs occur*/
List evalDefaults    = NIL;		/* defaults for evaluator	   */

Cell inputExpr	     = NIL;		/* input expression		   */
Bool literateScripts = FALSE;		/* TRUE => default to lit scripts  */
Bool literateErrors  = TRUE;		/* TRUE => report errs in lit scrs */

String repeatStr     = 0;		/* Repeat last expr		   */

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void local initCharTab	  Args((Void));
static Void local fileInput	  Args((String,Long));
static Bool local literateMode	  Args((String));
static Void local skip		  Args((Void));
static Void local thisLineIs	  Args((Int));
static Void local newlineSkip	  Args((Void));
static Void local closeAnyInput   Args((Void));

       Int  yyparse	    Args((Void)); /* can't stop yacc making this   */
					  /* public, but don't advertise   */
					  /* it in a header file.	   */

static Void local endToken	  Args((Void));
static Text local readOperator	  Args((Void));
static Text local readIdent	  Args((Void));
static Cell local readRadixNumber Args((Int));
static Cell local readNumber	  Args((Void));
static Cell local readChar	  Args((Void));
static Cell local readString	  Args((Void));
static Void local saveStrChr	  Args((Char));
static Cell local readAChar	  Args((Bool));

static Bool local lazyReadMatches Args((String));
static Cell local readEscapeChar  Args((Bool));
static Void local skipGap	  Args((Void));
static Cell local readCtrlChar	  Args((Void));
static Cell local readOctChar	  Args((Void));
static Cell local readHexChar	  Args((Void));
static Int  local readHexDigit	  Args((Char));
static Cell local readDecChar	  Args((Void));

static Void local goOffside	  Args((Int));
static Void local unOffside	  Args((Void));
static Bool local canUnOffside	  Args((Void));

static Void local skipWhitespace  Args((Void));
static Int  local yylex 	  Args((Void));
static Int  local repeatLast	  Args((Void));

static Void local parseInput	  Args((Int));

/* --------------------------------------------------------------------------
 * Text values for reserved words and special symbols:
 * ------------------------------------------------------------------------*/

static Text textCase,	 textOfK,      textData,   textType,   textIf;
static Text textThen,	 textElse,     textWhere,  textLet,    textIn;
static Text textInfix,   textInfixl,   textInfixr, textPrim,   textNewtype;
static Text textDefault, textDeriving, textDo,     textClass,  textInstance;

static Text textCoco,	 textEq,       textUpto,   textAs,     textLambda;
static Text textBar,	 textMinus,    textFrom,   textArrow,  textLazy;
static Text textBang,	 textImplies;

static Text textModule,  textImport;
static Text textHiding,  textQualified, textAsMod;

#if    LAZY_ST
static Text textRunST;
#endif

static Cell varMinus;			/* (-)				   */
static Cell varBang;			/* (!)				   */
static Cell varHiding;			/* hiding			   */
static Cell varQualified;		/* qualified			   */
static Cell varAsMod;			/* as				   */

#if    NPLUSK
Text   textPlus;			/* (+)				   */
#endif

static List imps;			/* List of imports to be chased	   */

/* --------------------------------------------------------------------------
 * Character set handling:
 *
 * Hugs follows Haskell 1.3 in assuming that input uses the ISO-8859-1
 * character set.  The following code provides methods for classifying
 * input characters according to the lexical structure specified by the
 * report.  Hugs should still accept older programs because ASCII is
 * essentially just a subset of the ISO character set.
 *
 * Notes: If you want to port Hugs to a machine that uses something
 * substantially different from the ISO character set, then you will need
 * to insert additional code to map between character sets.
 *
 * At some point, the following data structures may be exported in a .h
 * file to allow the information contained here to be picked up in the
 * implementation of LibChar is* primitives.
 *
 * Relies, implicitly but for this comment, on assumption that NUM_CHARS=256.
 * ------------------------------------------------------------------------*/

static  unsigned char   ctable[NUM_CHARS];
#define isIn(c,x)	(ctable[c]&(x))
#define isISO(c)	(0<=c && c<NUM_CHARS)

#define DIGIT		0x01
#define SMALL		0x02
#define LARGE		0x04
#define SYMBOL		0x08
#define IDAFTER		0x10
#define SPACE		0x20
#define PRINT		0x40

static Void local initCharTab() {	/* Initialize char decode table	   */
#define setRange(x,f,t)	{Int i=f;   while (i<=t) ctable[i++] |=x;}
#define setChars(x,s)	{char *p=s; while (*p)   ctable[*p++]|=x;}
#define setCopy(x,c)	{Int i;				\
			 for (i=0; i<NUM_CHARS; ++i)	\
			     if (isIn(i,c))		\
				 ctable[i]|=x;		\
			}

    setRange(DIGIT,	'0','9');	/* ASCII decimal digits		   */

    setRange(SMALL,	'a','z');	/* ASCII lower case letters	   */
    setRange(SMALL,	223,246);	/* ISO lower case letters	   */
    setRange(SMALL,	248,255);	/* (omits division symbol, 247)	   */

    setRange(LARGE,	'A','Z');	/* ASCII upper case letters	   */
    setRange(LARGE,	192,214);	/* ISO upper case letters	   */
    setRange(LARGE,	216,222);	/* (omits multiplication, 215)	   */

    setRange(SYMBOL,	161,191);	/* Symbol characters + ':'	   */
    setRange(SYMBOL,	215,215);
    setRange(SYMBOL,	247,247);
    setChars(SYMBOL,	":!#$%&*+./<=>?@\\^|-~");

    setChars(IDAFTER,	"'_");		/* Characters in identifier	   */
    setCopy (IDAFTER,	(DIGIT|SMALL|LARGE));

    setRange(SPACE,	' ',' ');	/* ASCII space character	   */
    setRange(SPACE,	160,160);	/* ISO non breaking space	   */
    setRange(SPACE,	9,13);		/* special whitespace: \t\n\v\f\r  */

    setChars(PRINT,	"(),;[]_`{}");	/* Special characters		   */
    setChars(PRINT,	" '\""); 	/* Space and quotes		   */
    setCopy (PRINT,	(DIGIT|SMALL|LARGE|SYMBOL));
#undef setRange
#undef setChars
#undef setCopy
}

/* --------------------------------------------------------------------------
 * Single character input routines:
 *
 * At the lowest level of input, characters are read one at a time, with the
 * current character held in c0 and the following (lookahead) character in
 * c1.	The corrdinates of c0 within the file are held in (column,row).
 * The input stream is advanced by one character using the skip() function.
 * ------------------------------------------------------------------------*/

#define TABSIZE    8		       /* spacing between tabstops	   */

#define NOTHING    0		       /* what kind of input is being read?*/
#define KEYBOARD   1		       /* - keyboard/console?		   */
#define SCRIPTFILE 2		       /* - script file 		   */
#define PROJFILE   3		       /* - project file		   */

static Int    reading	= NOTHING;

static Target readSoFar;
static Int    row, column, startColumn;
static int    c0, c1;
static FILE   *inputStream;
static Bool   thisLiterate;

#if     USE_READLINE			/* for command line editors	   */
static  String currentLine;		/* editline or GNU readline	   */
static  String nextChar;
#define nextConsoleChar()   (*nextChar=='\0' ? '\n' : *nextChar++)
extern  Void add_history    Args((String));
extern  String readline	    Args((String));
#else
#define nextConsoleChar()   getc(stdin)
#endif

static	Int litLines;		       /* count defn lines in lit script   */
#define DEFNCHAR  '>'		       /* definition lines begin with this */
static	Int lastLine;		       /* records type of last line read:  */
#define STARTLINE 0		       /* - at start of file, none read    */
#define BLANKLINE 1		       /* - blank (may preceed definition) */
#define TEXTLINE  2		       /* - text comment		   */
#define DEFNLINE  3		       /* - line containing definition	   */

Void consoleInput(prompt)		/* prepare to input characters from*/
String prompt; {			/* standard in (i.e. console/kbd)  */
    reading	= KEYBOARD;		/* keyboard input is Line oriented,*/
    c0		=			/* i.e. input terminated by '\n'   */
    c1		= ' ';
    column	= (-1);
    row 	= 0;

#if USE_READLINE
    if (currentLine)
	free(currentLine);
    currentLine = readline(prompt);
    nextChar    = currentLine;
    if (currentLine) {
	if (*currentLine)
	    add_history(currentLine);
    }
    else
	c0 = c1 = EOF;
#else
    printf("%s",prompt);
    fflush(stdout);
#endif
}

Void projInput(nm)		       /* prepare to input characters from */
String nm; {			       /* from named project file	   */
    if (inputStream = fopen(nm,"r")) {
	reading = PROJFILE;
	c0      = ' ';
	c1      = '\n';
        column  = 1;
        row     = 0;
    }
    else {
	ERRMSG(0) "Unable to open project file \"%s\"", nm
	EEND;
    }
}

static Void local fileInput(nm,len)	/* prepare to input characters from*/
String nm;				/* named file (specified length is */
Long   len; {				/* used to set target for reading) */
    if (inputStream = fopen(nm,"r")) {
	reading      = SCRIPTFILE;
	c0	     = ' ';
	c1	     = '\n';
	column	     = 1;
	row	     = 0;
	readSoFar    = 0;
	lastLine     = STARTLINE;
	litLines     = 0;
	thisLiterate = literateMode(nm);
	setGoal("Parsing", (Target)len);
    }
    else {
	ERRMSG(0) "Unable to open file \"%s\"", nm
	EEND;
    }
}

static Bool local literateMode(nm)	/* selecte literate mode for file  */
String nm; {
    String dot = 0;

#if !RISCOS
    for (; *nm; ++nm)			/* look for last dot in file name  */
	if (*nm == '.')
	    dot = nm+1;

    if (dot) {
	if (strcmp(dot,"hs")==0)	/* .hs files are never literate	   */
	    return FALSE;

	if (strcmp(dot,"lhs") ==0 ||	/* .lhs, .verb files are always	   */
	    strcmp(dot,"verb")==0)	/* literate scripts		   */
	    return TRUE;
    }
#else
    char *start = nm;
    for (; *nm; ++nm)                   /* look for last dot in file name  */
        if (*nm == '.')
            dot = nm+1;
    if (dot) {
	char *prev = dot-1;
	while (prev > start && *--prev != '.')
	    ;
	if (*prev == '.')
	    ++prev;
	if (namecmp(prev,"hs"))
	    return FALSE;
	if (namecmp(prev,"lhs") ||  namecmp(prev,"verb"))
	    return TRUE;
    }
#endif
    return literateScripts;		/* otherwise, use the default	   */
}

static Void local skip() {		/* move forward one char in input  */
    if (c0!=EOF) {			/* stream, updating c0, c1, ...	   */
	if (c0=='\n') {			/* Adjusting cursor coords as nec. */
	    row++;
	    column=1;
	    if (reading==SCRIPTFILE)
		soFar(readSoFar);
	}
	else if (c0=='\t')
	    column += TABSIZE - ((column-1)%TABSIZE);
	else
	    column++;

	c0 = c1;
	readSoFar++;

	if (c0==EOF) {
	    column = 0;
	    if (reading==SCRIPTFILE)
		done();
	    closeAnyInput();
	}
	else if (reading==KEYBOARD) {
	    allowBreak();
	    if (c0=='\n')
		c1 = EOF;
	    else
		c1 = nextConsoleChar();
	}
	else
	    c1 = getc(inputStream);
    }
}

static Void local thisLineIs(kind)	/* register kind of current line   */
Int kind; {				/* & check for literate script errs*/
    if (literateErrors) {
	if ((kind==DEFNLINE && lastLine==TEXTLINE) ||
	    (kind==TEXTLINE && lastLine==DEFNLINE)) {
	    ERRMSG(row) "Program line next to comment"
	    EEND;
	}
	lastLine = kind;
    }
}

static Void local newlineSkip() {      /* skip `\n' (supports lit scripts) */
    if (reading==SCRIPTFILE && thisLiterate) {
	do {
	    skip();
	    if (c0==DEFNCHAR) {        /* pass chars on definition lines   */
		thisLineIs(DEFNLINE);  /* to lexer (w/o leading DEFNCHAR)  */
		skip();
		litLines++;
		return;
	    }
	    while (c0==' ' || c0=='\t')/* maybe line is blank?		   */
		skip();
	    if (c0=='\n' || c0==EOF)
		thisLineIs(BLANKLINE);
	    else {
		thisLineIs(TEXTLINE);  /* otherwise it must be a comment   */
		while (c0!='\n' && c0!=EOF)
		    skip();
	    }			       /* by now, c0=='\n' or c0==EOF	   */
	} while (c0!=EOF);	       /* if new line, start again	   */

	if (litLines==0 && literateErrors) {
	    ERRMSG(row) "Empty script - perhaps you forgot the `%c's?",
		        DEFNCHAR
	    EEND;
	}
	return;
    }
    skip();
}

static Void local closeAnyInput() {	/* Close input stream, if open,	   */
    switch (reading) {			/* or skip to end of console line  */
	case PROJFILE   :
	case SCRIPTFILE : fclose(inputStream);
			  break;
	case KEYBOARD   : while (c0!=EOF)
			      skip();
			  break;
    }
    reading=NOTHING;
}

/* --------------------------------------------------------------------------
 * Parser: Uses table driven parser generated from parser.y using yacc
 * ------------------------------------------------------------------------*/

#include "parser.c"

/* --------------------------------------------------------------------------
 * Single token input routines:
 *
 * The following routines read the values of particular kinds of token given
 * that the first character of the token has already been located in c0 on
 * entry to the routine.
 * ------------------------------------------------------------------------*/

#define MAX_TOKEN	    250
#define startToken()	    tokPos = 0
#define saveTokenChar(c)    if (tokPos<=MAX_TOKEN) saveChar(c); else ++tokPos
#define saveChar(c)	    tokenStr[tokPos++]=(c)
#define overflows(n,b,d,m)  (n > ((m)-(d))/(b))

static char tokenStr[MAX_TOKEN+1];	/* token buffer			   */
static Int  tokPos;			/* input position in buffer	   */
static Int  identType;			/* identifier type: CONID / VARID  */
static Int  opType;			/* operator type  : CONOP / VAROP  */

static Void local endToken() {		/* check for token overflow	   */
    if (tokPos>MAX_TOKEN) {
	ERRMSG(row) "Maximum token length (%d) exceeded", MAX_TOKEN
	EEND;
    }
    tokenStr[tokPos] = '\0';
}

static Text local readOperator() {	/* read operator symbol		   */
    startToken();
    do {
	saveTokenChar(c0);
	skip();
    } while (isISO(c0) && isIn(c0,SYMBOL));
    opType = (tokenStr[0]==':' ? CONOP : VAROP);
    endToken();
    return findText(tokenStr);
}

static Text local readIdent() {		/* read identifier		   */
    startToken();
    do {
	saveTokenChar(c0);
	skip();
    } while (isISO(c0) && isIn(c0,IDAFTER));
    endToken();
    identType = isIn(tokenStr[0],LARGE) ? CONID : VARID;
    return findText(tokenStr);
}

static Cell local readRadixNumber(r)	/* Read literal in specified radix */
Int r; {				/* from input of the form 0c{digs} */
    Int d;
    skip();				/* skip leading zero		   */
    if ((d=readHexDigit(c1))<0 || d>=r) /* Special case; no digits, lex as */
	return mkInt(0);		/* if it had been written "0 c..." */
    else {
	Int  n = 0;
#if BIGNUMS
	Cell big = NIL;
#endif
	skip();
	do {
#if BIGNUMS
	    if (nonNull(big))
		big = bigShift(big,d,r);
	    else if (overflows(n,r,d,MAXPOSINT))
		big = bigShift(bigInt(n),d,r);
	    else
#else
	    if (overflows(n,r,d,MAXPOSINT)) {
		ERRMSG(row) "Integer literal out of range"
		EEND;
	    }
	    else
#endif
		n = r*n + d;
	    skip();
	    d = readHexDigit(c0);
	} while (d>=0 && d<r);
#if BIGNUMS
	return nonNull(big) ? big : mkInt(n);
#else
	return mkInt(n);
#endif
    }
}

static Cell local readNumber() {	/* read numeric constant	   */
    Int   n           = 0;
    Bool  intTooLarge = FALSE;

    if (c0=='0') {
	if (c1=='x' || c1=='X')		/* Maybe a hexadecimal literal?	   */
	    return readRadixNumber(16);
	if (c1=='o' || c1=='O')		/* Maybe an octal literal?	   */
	    return readRadixNumber(8);
    }

    startToken();
    do {
	if (overflows(n,10,(c0-'0'),MAXPOSINT))
	    intTooLarge = TRUE;
	n  = 10*n  + (c0-'0');
	saveTokenChar(c0);
	skip();
    } while (isISO(c0) && isIn(c0,DIGIT));

    if (c0!='.' || !isISO(c1) || !isIn(c1,DIGIT)) {
	endToken();
	if (!intTooLarge)
	    return mkInt(n);
#if BIGNUMS
	return bigStr(tokenStr);
#else
	ERRMSG(row) "Integer literal out of range"
	EEND;
#endif
    }

    saveTokenChar(c0);		        /* save decimal point		   */
    skip();
    do {				/* process fractional part ...	   */
	saveTokenChar(c0);
	skip();
    } while (isISO(c0) && isIn(c0,DIGIT));

    if (c0=='e' || c0=='E') {		/* look for exponent part...	   */
	saveTokenChar('e');
	skip();
	if (c0=='-') {
	    saveTokenChar('-');
	    skip();
	}
	else if (c0=='+')
	    skip();

	if (!isISO(c0) || !isIn(c0,DIGIT)) {
	    ERRMSG(row) "Missing digits in exponent"
	    EEND;
	}
	else {
	    do {
		saveTokenChar(c0);
		skip();
	    } while (isISO(c0) && isIn(c0,DIGIT));
	}
    }

    endToken();
#if !HAS_FLOATS
    ERRMSG(row) "No floating point numbers in this implementation"
    EEND;
#endif

    return mkFloat(stringToFloat(tokenStr));
}

static Cell local readChar() {	       /* read character constant	   */
    Cell charRead;

    skip(/* '\'' */);
    if (c0=='\'' || c0=='\n' || c0==EOF) {
	ERRMSG(row) "Illegal character constant"
	EEND;
    }

    charRead = readAChar(FALSE);

    if (c0=='\'')
	skip(/* '\'' */);
    else {
	ERRMSG(row) "Improperly terminated character constant"
	EEND;
    }
    return charRead;
}

static Cell local readString() {       /* read string literal		   */
    Cell c;

    startToken();
    skip(/* '\"' */);
    while (c0!='\"' && c0!='\n' && c0!=EOF) {
	c = readAChar(TRUE);
	if (nonNull(c))
	    saveStrChr(charOf(c));
    }

    if (c0=='\"')
	skip(/* '\"' */);
    else {
	ERRMSG(row) "Improperly terminated string"
	EEND;
    }
    endToken();
    return mkStr(findText(tokenStr));
}

static Void local saveStrChr(c)        /* save character in string	   */
Char c; {
    if (c!='\0' && c!='\\') {	       /* save non null char as single char*/
	saveTokenChar(c);
    }
    else {			       /* save null char as TWO null chars */
	if (tokPos+1<MAX_TOKEN) {
	    saveChar('\\');
	    if (c=='\\')
		saveChar('\\');
	    else
		saveChar('0');
	}
    }
}

static Cell local readAChar(isStrLit)	/* read single char constant	   */
Bool isStrLit; {			/* TRUE => enable \& and gaps	   */
    Cell c = mkChar(c0);

    if (c0=='\\')		       /* escape character?		   */
	return readEscapeChar(isStrLit);
    if (!isISO(c0) || !isIn(c0,PRINT)) {
	ERRMSG(row) "Non printable character `\\%d' in constant", ((int)c0)
	EEND;
    }
    skip();			       /* normal character?		   */
    return c;
}

/* --------------------------------------------------------------------------
 * Character escape code sequences:
 * ------------------------------------------------------------------------*/

static struct { 		       /* table of special escape codes    */
    char *codename;
    int  codenumber;
} escapes[] = {
   {"a",    7}, {"b",	 8}, {"f",   12}, {"n",   10},	/* common escapes  */
   {"r",   13}, {"t",	 9}, {"\\",'\\'}, {"\"",'\"'},
   {"\'",'\''}, {"v",	11},
   {"NUL",  0}, {"SOH",  1}, {"STX",  2}, {"ETX",  3},	/* ascii codenames */
   {"EOT",  4}, {"ENQ",  5}, {"ACK",  6}, {"BEL",  7},
   {"BS",   8}, {"HT",	 9}, {"LF",  10}, {"VT",  11},
   {"FF",  12}, {"CR",	13}, {"SO",  14}, {"SI",  15},
   {"DLE", 16}, {"DC1", 17}, {"DC2", 18}, {"DC3", 19},
   {"DC4", 20}, {"NAK", 21}, {"SYN", 22}, {"ETB", 23},
   {"CAN", 24}, {"EM",	25}, {"SUB", 26}, {"ESC", 27},
   {"FS",  28}, {"GS",	29}, {"RS",  30}, {"US",  31},
   {"SP",  32}, {"DEL", 127},
   {0,0}
};

static Int  alreadyMatched;	       /* Record portion of input stream   */
static char alreadyRead[10];	       /* that has been read w/o a match   */

static Bool local lazyReadMatches(s)   /* compare input stream with string */
String s; {			       /* possibly using characters that   */
    int i;			       /* have already been read	   */

    for (i=0; i<alreadyMatched; ++i)
	if (alreadyRead[i]!=s[i])
	    return FALSE;

    while (s[i] && s[i]==c0) {
	alreadyRead[alreadyMatched++]=c0;
	skip();
	i++;
    }

    return s[i]=='\0';
}

static Cell local readEscapeChar(isStrLit)/* read escape character	   */
Bool isStrLit; {
    int i=0;

    skip(/* '\\' */);
    switch (c0) {
	case '&'  : if (isStrLit) {
			skip();
			return NIL;
		    }
		    ERRMSG(row) "Illegal use of `\\&' in character constant"
		    EEND;
		    break;/*NOTREACHED*/

	case '^'  : return readCtrlChar();

	case 'o'  : return readOctChar();
	case 'x'  : return readHexChar();

	default	  : if (!isISO(c0)) {
			ERRMSG(row) "Illegal escape sequence"
			EEND;
		    }
		    else if (isIn(c0,SPACE)) {
			if (isStrLit) {
			    skipGap();
			    return NIL;
			}
			ERRMSG(row) "Illegal use of gap in character constant"
			EEND;
			break;
		    }
		    else if (isIn(c0,DIGIT))
			return readDecChar();
    }

    for (alreadyMatched=0; escapes[i].codename; i++)
	if (lazyReadMatches(escapes[i].codename))
	    return mkChar(escapes[i].codenumber);

    alreadyRead[alreadyMatched++] = c0;
    alreadyRead[alreadyMatched++] = '\0';
    ERRMSG(row) "Illegal character escape sequence \"\\%s\"",
	        alreadyRead
    EEND;
    return NIL;/*NOTREACHED*/
}

static Void local skipGap() {		/* skip over gap in string literal */
    do					/* (simplified in Haskell 1.1)	   */
	if (c0=='\n')
	    newlineSkip();
	else
	    skip();
    while (isISO(c0) && isIn(c0,SPACE));
    if (c0!='\\') {
	ERRMSG(row) "Missing `\\' terminating string literal gap"
	EEND;
    }
    skip(/* '\\' */);
}

static Cell local readCtrlChar() {     /* read escape sequence \^x	   */
    static String controls = "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_";
    String which;

    skip(/* '^' */);
    if ((which = strchr(controls,c0))==NULL) {
	ERRMSG(row) "Unrecognised escape sequence `\\^%c'", c0
	EEND;
    }
    skip();
    return mkChar(which-controls);
}

static Cell local readOctChar() {      /* read octal character constant    */
    Int n = 0;
    Int d;

    skip(/* 'o' */);
    if ((d = readHexDigit(c0))<0 || d>=8) {
	ERRMSG(row) "Empty octal character escape"
	EEND;
    }
    do {
	if (overflows(n,8,d,MAXCHARVAL)) {
	    ERRMSG(row) "Octal character escape out of range"
	    EEND;
	}
	n = 8*n + d;
	skip();
    } while ((d = readHexDigit(c0))>=0 && d<8);

    return mkChar(n);
}

static Cell local readHexChar() {      /* read hex character constant	   */
    Int n = 0;
    Int d;

    skip(/* 'x' */);
    if ((d = readHexDigit(c0))<0) {
	ERRMSG(row) "Empty hexadecimal character escape"
	EEND;
    }
    do {
	if (overflows(n,16,d,MAXCHARVAL)) {
	    ERRMSG(row) "Hexadecimal character escape out of range"
	    EEND;
	}
	n = 16*n + d;
	skip();
    } while ((d = readHexDigit(c0))>=0);

    return mkChar(n);
}

static Int local readHexDigit(c)	/* read single hex digit 	   */
Char c; {
    if ('0'<=c && c<='9')
	return c-'0';
    if ('A'<=c && c<='F')
	return 10 + (c-'A');
    if ('a'<=c && c<='f')
	return 10 + (c-'a');
    return -1;
}

static Cell local readDecChar() {      /* read decimal character constant  */
    Int n = 0;

    do {
	if (overflows(n,10,(c0-'0'),MAXCHARVAL)) {
	    ERRMSG(row) "Decimal character escape out of range"
	    EEND;
	}
	n = 10*n + (c0-'0');
	skip();
    } while (c0!=EOF && isIn(c0,DIGIT));

    return mkChar(n);
}

/* --------------------------------------------------------------------------
 * Produce printable representation of character:
 * ------------------------------------------------------------------------*/

String unlexChar(c,quote)		/* return string representation of */
Char c;					/* character...			   */
Char quote; {				/* protect quote character	   */
    static char buffer[12];

    if (c<0)				/* deal with sign extended chars.. */
	c += NUM_CHARS;

    if (isISO(c) && isIn(c,PRINT)) {	/* normal printable character	   */
	if (c==quote || c=='\\') {	/* look for quote of approp. kind  */
	    buffer[0] = '\\';		
	    buffer[1] = c;
	    buffer[2] = '\0';
	}
	else {
	    buffer[0] = c;
            buffer[1] = '\0';
	}
    }
    else {				/* look for escape code		   */
        Int escs;
        for (escs=0; escapes[escs].codename; escs++)
	    if (escapes[escs].codenumber==c) {
		sprintf(buffer,"\\%s",escapes[escs].codename);
		return buffer;
	    }
        sprintf(buffer,"\\%d",c);	/* otherwise use numeric escape	   */
    }
    return buffer;
}

Void printString(s)			/* print string s, using quotes and*/
String s; {				/* escapes if any parts need them  */
    if (s) {
	String t = s;
	while (*t && isISO(*t) && isIn(*t,PRINT) && *t!='"' && !isIn(*t,SPACE))
	    t++;
	if (*t) {
	    putchar('"');
	    for (t=s; *t; t++)
		printf("%s",unlexChar(*t,'"'));
	    putchar('"');
	}
	else
	    printf("%s",s);
    }
}

/* --------------------------------------------------------------------------
 * Handle special types of input for use in interpreter:
 * ------------------------------------------------------------------------*/

Command readCommand(cmds,start,sys)	/* read command at start of input  */
struct cmd *cmds;			/* line in interpreter		   */
Char   start;				/* characters introducing a cmd    */
Char   sys; {				/* character for shell escape	   */
    while (c0==' ' || c0 =='\t')
	skip();

    if (c0=='\n')			/* look for blank command lines    */
	return NOCMD;
    if (c0==EOF)			/* look for end of input stream	   */
	return QUIT;
    if (c0==sys) {			/* single character system escape  */
	skip();
	return SYSTEM;
    }
    if (c0==start && c1==sys) {		/* two character system escape	   */
	skip();
	skip();
	return SYSTEM;
    }

    startToken();			/* All cmds start with start	   */
    if (c0==start)			/* except default (usually EVAL)   */
	do {				/* which is empty		   */
	    saveTokenChar(c0);
	    skip();
	} while (c0!=EOF && !isIn(c0,SPACE));
    endToken();

    for (; cmds->cmdString; ++cmds)
	if (strcmp((cmds->cmdString),tokenStr)==0 ||
            (tokenStr[0]==start &&
             tokenStr[1]==(cmds->cmdString)[1] &&
             tokenStr[2]=='\0'))
	    return (cmds->cmdCode);
    return BADCMD;
}

String readFilename() { 	       /* Read filename from input (if any)*/
    if (reading==PROJFILE)
	skipWhitespace();
    else
	while (c0==' ' || c0=='\t')
	    skip();

    if (c0=='\n' || c0==EOF)	       /* return null string at end of line*/
	return 0;

    startToken();
    while (c0!=EOF && !isIn(c0,SPACE)) {
	if (c0=='"') {
	    skip();
	    while (c0!=EOF && c0!='\"') {
		Cell c = readAChar(TRUE);
		if (nonNull(c))
		    saveTokenChar(charOf(c));
	    }
	    if (c0=='"')
		skip();
	    else {
		ERRMSG(row) "a closing quote, '\"', was expected"
		EEND;
	    }
	}
	else {
	    saveTokenChar(c0);
	    skip();
	}
    }
    endToken();
    return tokenStr;
}

String readLine() {			/* Read command line from input	   */
    while (c0==' ' || c0=='\t')		/* skip leading whitespace	   */
	skip();

    startToken();
    while (c0!='\n' && c0!=EOF) {
	saveTokenChar(c0);
	skip();
    }
    endToken();

    return tokenStr;
}

/* --------------------------------------------------------------------------
 * This lexer supports the Haskell layout rule:
 *
 * - Layout area bounded by { ... }, with `;'s in between.
 * - A `{' is a HARD indentation and can only be matched by a corresponding
 *   HARD '}'
 * - Otherwise, if no `{' follows the keywords WHERE/LET or OF, a SOFT `{'
 *   is inserted with the column number of the first token after the
 *   WHERE/LET/OF keyword.
 * - When a soft indentation is uppermost on the indetation stack with
 *   column col' we insert:
 *    `}'  in front of token with column<col' and pop indentation off stack,
 *    `;'  in front of token with column==col'.
 * ------------------------------------------------------------------------*/

#define MAXINDENT  100		       /* maximum nesting of layout rule   */
static	Int	   layout[MAXINDENT+1];/* indentation stack		   */
#define HARD	   (-1) 	       /* indicates hard indentation	   */
static	Int	   indentDepth = (-1); /* current indentation nesting	   */

static Void local goOffside(col)       /* insert offside marker 	   */
Int col; {			       /* for specified column		   */
    if (indentDepth>=MAXINDENT) {
	ERRMSG(row) "Too many levels of program nesting"
	EEND;
    }
    layout[++indentDepth] = col;
}

static Void local unOffside() {        /* leave layout rule area	   */
    indentDepth--;
}

static Bool local canUnOffside() {     /* Decide if unoffside permitted    */
    return indentDepth>=0 && layout[indentDepth]!=HARD;
}

/* --------------------------------------------------------------------------
 * Main tokeniser:
 * ------------------------------------------------------------------------*/

static Void local skipWhitespace() {	/* Skip over whitespace/comments   */
    for (;;)				/* Strictly speaking, this code is */
	if (c0==EOF)			/* a little more liberal than the  */
	    return;			/* report allows ...		   */
	else if (c0=='\n')
	    newlineSkip();
	else if (isIn(c0,SPACE))
	    skip();
	else if (c0=='{' && c1=='-') {	/* (potentially) nested comment	   */
	    Int nesting = 1;
	    Int origRow = row;		/* Save original row number	   */
	    skip();
	    skip();
	    while (nesting>0 && c0!=EOF)
		if (c0=='{' && c1=='-') {
		    skip();
		    skip();
		    nesting++;
		}
		else if (c0=='-' && c1=='}') {
		    skip();
		    skip();
		    nesting--;
		}
		else if (c0=='\n')
		    newlineSkip();
		else
		    skip();
	    if (nesting>0) {
		ERRMSG(origRow) "Unterminated nested comment {- ..."
		EEND;
	    }
	}
	else if (c0=='-' && c1=='-') {	/* One line comment		   */
	    do
		skip();
	    while (c0!='\n' && c0!=EOF);
	    if (c0=='\n')
		newlineSkip();
	}
	else
	    return;
}

static Bool firstToken; 	       /* Set to TRUE for first token	   */
static Int  firstTokenIs;	       /* ... with token value stored here */

static Int local yylex() {	       /* Read next input token ...	   */
    static Bool insertOpen    = FALSE;
    static Bool insertedToken = FALSE;
    static Text textRepeat;

#define lookAhead(t) {skipWhitespace(); insertOpen = (c0!='{'); return t;}

    if (firstToken) {		       /* Special case for first token	   */
	indentDepth   = (-1);
	firstToken    = FALSE;
	insertOpen    = FALSE;
	insertedToken = FALSE;
	if (reading==KEYBOARD)
	    textRepeat = findText(repeatStr);
	return firstTokenIs;
    }

    if (insertOpen) {		       /* insert `soft' opening brace	   */
	insertOpen    = FALSE;
	insertedToken = TRUE;
	goOffside(column);
	push(yylval = mkInt(row));
	return '{';
    }

    /* ----------------------------------------------------------------------
     * Skip white space, and insert tokens to support layout rules as reqd.
     * --------------------------------------------------------------------*/

    skipWhitespace();
    startColumn = column;
    push(yylval = mkInt(row));	       /* default token value is line no.  */
    /* subsequent changes to yylval must also set top() to the same value  */

    if (indentDepth>=0) 	       /* layout rule(s) active ?	   */
        if (insertedToken)	       /* avoid inserting multiple `;'s    */
	    insertedToken = FALSE;     /* or putting `;' after `{'	   */
        else if (layout[indentDepth]!=HARD)
	    if (column<layout[indentDepth]) {
		unOffside();
		return '}';
            }
            else if (column==layout[indentDepth] && c0!=EOF) {
                insertedToken = TRUE;
                return ';';
            }

    /* ----------------------------------------------------------------------
     * Now try to identify token type:
     * --------------------------------------------------------------------*/

    switch (c0) {
	case EOF  : return 0;			/* End of file/input	   */

	/* The next 10 characters make up the `special' category in 1.3	   */
	case '('  : skip(); return '(';
	case ')'  : skip(); return ')';
	case ','  : skip(); return ',';
	case ';'  : skip(); return ';'; 
	case '['  : skip(); return '['; 
	case ']'  : skip(); return ']';
	case '_'  : skip(); return '_';
	case '`'  : skip(); return '`';
	case '{'  : goOffside(HARD);
		    skip();
		    return '{';
	case '}'  : if (indentDepth<0) {
			ERRMSG(row) "Misplaced `}'"
			EEND;
		    }
		    if (layout[indentDepth]==HARD)	/* skip over hard }*/
			skip();
		    unOffside();	/* otherwise, we have to insert a }*/
		    return '}';		/* to (try to) avoid an error...   */

	/* Character and string literals				   */
	case '\'' : top() = yylval = readChar();
		    return CHARLIT;

	case '\"' : top() = yylval = readString();
		    return STRINGLIT;
    }

    if (isIn(c0,LARGE)) {		/* Look for, but ignore, qual name */
	Text it = readIdent();		/* No keyword begins with LARGE ...*/
	if (c0=='.' && isIn(c1,(SMALL|LARGE|SYMBOL)))
	    skip();			/* Skip qualifying dot		   */
	else {
	    top() = yylval = ap(CONIDCELL,it);
	    return identType;
	}				/* We could easily keep a record of*/
    }					/* the qualifying name here ...    */
    if (isIn(c0,(SMALL|LARGE))) {
	Text it = readIdent();

	if (it==textCase)              return CASEXP;
	if (it==textOfK)               lookAhead(OF);
	if (it==textData)	       return DATA;
	if (it==textType)	       return TYPE;
	if (it==textIf) 	       return IF;
	if (it==textThen)	       return THEN;
	if (it==textElse)	       return ELSE;
	if (it==textWhere)             lookAhead(WHERE);
        if (it==textLet)               lookAhead(LET);
        if (it==textIn)                return IN;
	if (it==textInfix)	       return INFIX;
	if (it==textInfixl)	       return INFIXL;
	if (it==textInfixr)	       return INFIXR;
	if (it==textPrim)              return PRIMITIVE;
	if (it==textNewtype)	       return TNEWTYPE;
	if (it==textDefault)	       return DEFAULT;
	if (it==textDeriving)	       return DERIVING;
	if (it==textDo)		       lookAhead(DO);
	if (it==textClass)	       return TCLASS;
	if (it==textInstance)	       return TINSTANCE;
	if (it==textModule)	       return MODULE;
	if (it==textImport)	       return IMPORT;
	if (it==textHiding)	       return HIDING;
	if (it==textQualified)	       return QUALIFIED;
	if (it==textAsMod)	       return ASMOD;
	if (it==textRepeat && reading==KEYBOARD)
	    return repeatLast();
#if LAZY_ST
	if (it==textRunST)	       return TRUNST;
#endif

	top() = yylval = ap((identType==CONID ? CONIDCELL : VARIDCELL),it);
	return identType;
    }

    if (isIn(c0,SYMBOL)) {
	Text it = readOperator();

	if (it==textCoco)    return COCO;
	if (it==textEq)      return '=';
	if (it==textUpto)    return UPTO;
	if (it==textAs)      return '@';
	if (it==textLambda)  return '\\';
	if (it==textBar)     return '|';
	if (it==textFrom)    return FROM;
	if (it==textMinus)   return '-';
	if (it==textBang)    return '!';
	if (it==textArrow)   return ARROW;
	if (it==textLazy)    return '~';
	if (it==textImplies) return IMPLIES;
	if (it==textRepeat && reading==KEYBOARD)
	    return repeatLast();

	top() = yylval = ap((opType==CONOP ? CONOPCELL : VAROPCELL),it);
	return opType;
    }

    if (isIn(c0,DIGIT)) {
	top() = yylval = readNumber();
	return NUMLIT;
    }

    ERRMSG(row) "Unrecognised character `\\%d' in column %d", ((int)c0), column
    EEND;
    return 0; /*NOTREACHED*/
}

static Int local repeatLast() {		/* Obtain last expression entered  */
    if (isNull(yylval=getLastExpr())) {
	ERRMSG(row) "Cannot use %s without any previous input", repeatStr
	EEND;
    }
    return REPEAT;
}

Syntax defaultSyntax(t) 	       /* Find default syntax of var named */
Text t; {			       /* by t ...			   */
    String s = textToStr(t);
    return isIn(s[0],SYMBOL) ? DEF_OPSYNTAX : APPLIC;
}

/* --------------------------------------------------------------------------
 * main entry points to parser/lexer:
 * ------------------------------------------------------------------------*/

static Void local parseInput(startWith)/* Parse input with given first tok,*/
Int startWith; {		       /* determining whether to read a    */
    firstToken	 = TRUE;	       /* script or an expression	   */
    firstTokenIs = startWith;

    clearStack();
    if (yyparse()) {		       /* This can only be parser overflow */
	ERRMSG(row) "Parser overflow"  /* as all syntax errors are caught  */
	EEND;			       /* in the parser...		   */
    }
    drop();
    if (!stackEmpty())		       /* stack should now be empty	   */
	internal("parseInput");
}

Void parseScript(nm,len)	       /* Read a script			   */
String nm;
Long   len; {			       /* Used to set a target for reading */
    input(RESET);
    fileInput(nm,len);
    parseInput(SCRIPT);
}

Void parseExp() {		       /* Read an expression to evaluate   */
    parseInput(EXPR);
    setLastExpr(inputExpr);
}

/* --------------------------------------------------------------------------
 * Input control:
 * ------------------------------------------------------------------------*/

Void input(what)
Int what; {
    switch (what) {
	case INSTALL : initCharTab();
		       textCase       = findText("case");
		       textOfK	      = findText("of");
		       textData       = findText("data");
		       textType       = findText("type");
		       textIf	      = findText("if");
		       textThen       = findText("then");
		       textElse       = findText("else");
		       textWhere      = findText("where");
                       textLet        = findText("let");
		       textIn         = findText("in");
		       textInfix      = findText("infix");
		       textInfixl     = findText("infixl");
		       textInfixr     = findText("infixr");
		       textPrim       = findText("primitive");
		       textNewtype    = findText("newtype");
		       textDefault    = findText("default");
		       textDeriving   = findText("deriving");
		       textDo	      = findText("do");
		       textClass      = findText("class");
		       textInstance   = findText("instance");
		       textCoco       = findText("::");
		       textEq	      = findText("=");
		       textUpto       = findText("..");
		       textAs	      = findText("@");
		       textLambda     = findText("\\");
		       textBar	      = findText("|");
		       textMinus      = findText("-");
		       textFrom       = findText("<-");
		       textArrow      = findText("->");
		       textLazy       = findText("~");
		       textBang	      = findText("!");
		       textImplies    = findText("=>");
#if NPLUSK
		       textPlus	      = findText("+");
#endif
		       textModule     = findText("module");
		       textImport     = findText("import");
		       textHiding     = findText("hiding");
		       textQualified  = findText("qualified");
		       textAsMod      = findText("as");
#if LAZY_ST
		       textRunST      = findText("runST");
#endif
		       varMinus	      = mkVar(textMinus);
		       varBang	      = mkVar(textBang);
		       varHiding      = mkVar(textHiding);
		       varQualified   = mkVar(textQualified);
		       varAsMod       = mkVar(textAsMod);
		       evalDefaults   = NIL;

		       input(RESET);
		       break;

	case RESET   : tyconDefns   = NIL;
		       typeInDefns  = NIL;
		       valDefns     = NIL;
		       opDefns	    = NIL;
		       classDefns   = NIL;
                       instDefns    = NIL;
		       selDefns	    = NIL;
		       overDefns    = NIL;
		       primDefns    = NIL;
		       defaultDefns = NIL;
		       defaultLine  = 0;
		       inputExpr    = NIL;
		       imps         = NIL;
		       closeAnyInput();
		       break;

	case BREAK   : if (reading==KEYBOARD)
			   c0 = EOF;
		       break;

	case MARK    : mark(tyconDefns);
		       mark(typeInDefns);
		       mark(valDefns);
		       mark(opDefns);
		       mark(classDefns);
                       mark(instDefns);
                       mark(selDefns);
		       mark(overDefns);
		       mark(primDefns);
		       mark(defaultDefns);
		       mark(evalDefaults);
		       mark(inputExpr);
		       mark(varMinus);
		       mark(varBang);
		       mark(varHiding);
		       mark(varQualified);
		       mark(varAsMod);
		       mark(imps);
		       break;
    }
}

/*-------------------------------------------------------------------------*/
