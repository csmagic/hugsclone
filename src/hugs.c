/* --------------------------------------------------------------------------
 * hugs.c:	Copyright (c) Mark P Jones 1991-1996.   All rights reserved.
 *		See NOTICE for details and conditions of use etc...
 *		Hugs version 1.3, August 1996
 *
 * Command interpreter
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "command.h"
#include "connect.h"
#include "errors.h"
#include <setjmp.h>
#include <ctype.h>

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void   local initialize	      Args((Int,String []));
static Void   local interpreter       Args((Int,String []));
static Void   local menu	      Args((Void));
static Void   local guidance	      Args((Void));
static Void   local forHelp	      Args((Void));
static Void   local set		      Args((Void));
static Void   local changeDir	      Args((Void));
static Void   local load	      Args((Void));
static Void   local project           Args((Void));
static Void   local readScripts       Args((Int));
static Void   local whatFiles	      Args((Void));
static Void   local editor	      Args((Void));
static Void   local find	      Args((Void));
static Void   local runEditor         Args((Void));
static Void   local evaluator	      Args((Void));
static Void   local evalExp	      Args((Void));
static Void   local stopAnyPrinting   Args((Void));
static Void   local showtype	      Args((Void));
static Void   local info	      Args((Void));
static Void   local showInst	      Args((Inst));
static Void   local describe	      Args((Text));
static Void   local listNames	      Args((Void));

static Void   local toggleSet	      Args((Char,Bool));
static Void   local togglesIn	      Args((Bool));
static Void   local optionInfo	      Args((Void));
static Bool   local processOption     Args((String));
static Int    local argToInt	      Args((String));

static Void   local loadProject       Args((String));
static Void   local clearProject      Args((Void));
static Void   local addScriptName     Args((String,Bool));
static Bool   local addScript	      Args((String,Long));
static Void   local forgetScriptsFrom Args((Module));
static Void   local setLastEdit       Args((String,Int));
static Void   local failed	      Args((Void));
static String local strCopy	      Args((String));

/* --------------------------------------------------------------------------
 * Machine dependent code for Hugs interpreter:
 * ------------------------------------------------------------------------*/

#include "machdep.c"
#ifdef WANT_TIMER
#include "timer.c"
#endif

/* --------------------------------------------------------------------------
 * Local data areas:
 * ------------------------------------------------------------------------*/

static Bool   printing	   = FALSE; 	/* TRUE => currently printing value*/
static Bool   showStats	   = FALSE; 	/* TRUE => print stats after eval  */
static Bool   listFiles	   = TRUE;  	/* TRUE => list files after loading*/
static Bool   addType	   = FALSE; 	/* TRUE => print type with value   */
static Bool   useShow	   = TRUE;  	/* TRUE => use Text/show printer   */
static Bool   chaseImports = TRUE;	/* TRUE => chase imports on load   */
static Bool   useDots      = RISCOS;	/* TRUE => use dots in progress    */

static String scriptName[NUM_SCRIPTS];	/* Script file names		   */
static Time   lastChange[NUM_SCRIPTS];	/* Time of last change to file	   */
static Bool   postponed[NUM_SCRIPTS];   /* Indicates postponed load	   */
static Int    numScripts;		/* Number of scripts loaded	   */
static Int    namesUpto;		/* Number of script names set	   */
static Bool   needsImports;		/* set to TRUE if imports required */
static String scriptFile;		/* Name of current script (if any) */

static String currProject = 0;		/* Name of current project file	   */
static Bool   projectLoaded = FALSE;	/* TRUE => project file loaded	   */

static String lastEdit	 = 0;		/* Name of file to edit (if any)   */
static Int    lastLine	 = 0;		/* Editor line number (if possible)*/
static String prompt     = 0;		/* Prompt string		   */

String hugsEdit = 0;			/* String for editor command       */
String hugsPath = 0;			/* String for file search path     */

/* --------------------------------------------------------------------------
 * Hugs entry point:
 * ------------------------------------------------------------------------*/

Main main Args((Int, String []));	/* now every func has a prototype  */

Main main(argc,argv)
int  argc;
char *argv[]; {
    CStackBase = &argc;                 /* Save stack base for use in gc   */

    /* The startup banner now includes my name.  Hugs is provided free of  */
    /* charge.  I ask however that you show your appreciation for the many */
    /* hours of work involved by retaining my name in the banner.  Thanks! */

#ifdef I_REALLY_DONT_WANT_THE_BIG_BANNER_PLEASE_USE_THE_LESS_OBNOXIOUS_VERSION
    printf("Hugs, The Haskell User's Gofer System Version 1.3beta\n");
    printf("Copyright (c) Mark P Jones, The University of Nottingham, 1994-1996.\n\n");
#else
    printf("      ___    ___   ___    ___   __________   __________\n");
    printf("     /  /   /  /  /  /   /  /  /  _______/  /  _______/   The Haskell User's\n");
    printf("    /  /___/  /  /  /   /  /  /  / _____   /  /______        Gofer System\n");
    printf("   /  ____   /  /  /   /  /  /  / /_   /  /______   /\n");
    printf("  /  /   /  /  /  /___/  /  /  /___/  /  _______/  /          Version 1.3\n");
    printf(" /__/   /__/  /_________/  /_________/  /_________/           August 1996\n\n");
    printf("     Copyright (c) Mark P Jones, The University of Nottingham, 1994-1996.\n\n");
#endif
    fflush(stdout);
    interpreter(argc,argv);
    printf("[Leaving Hugs]\n");
    everybody(EXIT);
    exit(0);
    MainDone
}

/* --------------------------------------------------------------------------
 * Initialization, interpret command line args and read prelude:
 * ------------------------------------------------------------------------*/

static Void local initialize(argc,argv)/* Interpreter initialization	   */
Int    argc;
String argv[]; {
    Module i;
    String proj = 0;

    setLastEdit((String)0,0);
    lastEdit	  = 0;
    scriptFile	  = 0;
    numScripts	  = 0;
    namesUpto	  = 1;
    hugsPath      = strCopy(fromEnv("HUGSPATH",NULL));
    hugsEdit      = strCopy(fromEnv("HUGSEDIT",fromEnv("EDITOR",NULL)));
    prompt	  = strCopy("? ");
    repeatStr	  = strCopy("$$");

    for (i=1; i<argc; ++i)		/* process command line arguments  */
	if (strcmp(argv[i],"+")==0 && i+1<argc)
	    if (proj) {
		ERRMSG(0) "Multiple project filenames on command line"
		EEND;
	    }
	    else
		proj = argv[++i];
	else if (!processOption(argv[i]))
	    addScriptName(argv[i],TRUE);

    scriptName[0] = strCopy(findPathname(NULL,STD_PRELUDE));

    everybody(INSTALL);
    if (proj) {
	if (namesUpto>1)
	    fprintf(stderr,
		    "\nUsing project file, ignoring additional filenames\n");
	loadProject(strCopy(proj));
    }
    readScripts(0);
}

/* --------------------------------------------------------------------------
 * Command line options:
 * ------------------------------------------------------------------------*/

struct options {			/* command line option toggles	   */
    char   c;				/* table defined in main app.	   */
    String description;
    Bool   *flag;
};
extern struct options toggle[];

static Void local toggleSet(c,state)	/* Set command line toggle	   */
Char c;
Bool state; {
    Int i;
    for (i=0; toggle[i].c; ++i)
	if (toggle[i].c == c) {
	    *toggle[i].flag = state;
	    return;
	}
    ERRMSG(0) "Unknown toggle `%c'", c
    EEND;
}

static Void local togglesIn(state)	/* Print current list of toggles in*/
Bool state; {				/* given state			   */
    Int count = 0;
    Int i;
    for (i=0; toggle[i].c; ++i)
	if (*toggle[i].flag == state) {
	    if (count==0)
		putchar(state ? '+' : '-');
	    putchar(toggle[i].c);
	    count++;
	}
    if (count>0)
	putchar(' ');
}

static Void local optionInfo() {	/* Print information about command */
    static String fmts = "%-5s%s\n";	/* line settings		   */
    static String fmtc = "%-5c%s\n";
    Int    i;

    printf("TOGGLES: groups begin with +/- to turn options on/off resp.\n");
    for (i=0; toggle[i].c; ++i)
	printf(fmtc,toggle[i].c,toggle[i].description);

    printf("\nOTHER OPTIONS: (leading + or - makes no difference)\n");
    printf(fmts,"hnum","Set heap size (cannot be changed within Hugs)");
    printf(fmts,"pstr","Set prompt string to str");
    printf(fmts,"rstr","Set repeat last expression string to str");
    printf(fmts,"Pstr","Set search path for script files to str");
    printf(fmts,"Estr","Use editor setting given by str");

    printf("\nCurrent settings: ");
    togglesIn(TRUE);
    togglesIn(FALSE);
    printf("-h%d",heapSize);
    printf(" -p");
    printString(prompt);
    printf(" -r");
    printString(repeatStr);
    printf("\nSearch path     : -P");
    printString(hugsPath);
    printf("\nEditor setting  : -E");
    printString(hugsEdit);
    putchar('\n');
}

static Bool local processOption(s)	/* process string s for options,   */
String s; {				/* return FALSE if none found.	   */
    Bool state;

    if (s[0]=='-')
	state = FALSE;
    else if (s[0]=='+')
	state = TRUE;
    else
	return FALSE;

    while (*++s)
	switch (*s) {
	    case 'p' : if (s[1]) {
			   if (prompt) free(prompt);
			   prompt = strCopy(s+1);
		       }
		       return TRUE;

	    case 'r' : if (s[1]) {
			   if (repeatStr) free(repeatStr);
			   repeatStr = strCopy(s+1);
		       }
		       return TRUE;

	    case 'P' : if (hugsPath) free(hugsPath);
		       hugsPath = strCopy(s+1);
		       return TRUE;

	    case 'E' : if (hugsEdit) free(hugsEdit);
		       hugsEdit = strCopy(s+1);
		       return TRUE;

	    case 'h' : if (heapBuilt()) {
			   ERRMSG(0) "Cannot change heap size"
			   EEND;
		       }
		       heapSize = argToInt(s);
		       if (heapSize<MINIMUMHEAP)
			   heapSize = MINIMUMHEAP;
		       else if (MAXIMUMHEAP && heapSize>MAXIMUMHEAP)
			   heapSize = MAXIMUMHEAP;
		       return TRUE;

	    default  : toggleSet(*s,state);
		       break;
	}
    return TRUE;
}

static Int local argToInt(s)		/* read integer from argument str  */
String s; {
    Int    n = 0;
    String t = s++;

    if (*s=='\0' || !isascii(*s) || !isdigit(*s)) {
	ERRMSG(0) "Missing integer in option setting \"%s\"", t
	EEND;
    }

    do {
	Int d = (*s++) - '0';
	if (n > ((MAXPOSINT - d)/10)) {
	    ERRMSG(0) "Option setting \"%s\" is too large", t
	    EEND;
	}
	n     = 10*n + d;
    } while (isascii(*s) && isdigit(*s));

    if (*s=='K' || *s=='k') {
	if (n > (MAXPOSINT/1000)) {
	    ERRMSG(0) "Option setting \"%s\" is too large", t
	    EEND;
	}
	n *= 1000;
	s++;
    }

    if (*s!='\0') {
	ERRMSG(0) "Unwanted characters after option setting \"%s\"", t
	EEND;
    }

    return n;
}

/* --------------------------------------------------------------------------
 * Print Menu of list of commands:
 * ------------------------------------------------------------------------*/

static struct cmd cmds[] = {
 {":?",	     HELP},   {":cd",   CHGDIR},  {":also",    ALSO},
 {":type",   TYPEOF}, {":!",    SYSTEM},  {":load",    LOAD},
 {":reload", RELOAD}, {":gc",   COLLECT}, {":edit",    EDIT},
 {":quit",   QUIT},   {":set",  SET},     {":find",    FIND},
 {":names",  NAMES},  {":info",	INFO},	  {":project", PROJECT},
 {"",        EVAL},
 {0,0}
};

static Void local menu() {
    printf("LIST OF COMMANDS:  Any command may be abbreviated to :c where\n");
    printf("c is the first character in the full name.\n\n");
    printf(":load <filenames>   load scripts from specified files\n");
    printf(":load               clear all files except prelude\n");
    printf(":also <filenames>   read additional script files\n");
    printf(":reload             repeat last load command\n");
    printf(":project <filename> use project file\n");
    printf(":edit <filename>    edit file\n");
    printf(":edit               edit last file\n");
    printf("<expr>              evaluate expression\n");
    printf(":type <expr>        print type of expression\n");
    printf(":?                  display this list of commands\n");
    printf(":set <options>      set command line options\n");
    printf(":set                help on command line options\n");
    printf(":names [pat]        list names currently in scope\n");
    printf(":info <names>       describe named objects\n");
    printf(":find <name>        edit file containing definition of name\n");
    printf(":!command           shell escape\n");
    printf(":cd dir             change directory\n");
    printf(":gc                 force garbage collection\n");
    printf(":quit               exit Hugs interpreter\n");
}

static Void local guidance() {
    printf("Command not recognised.  ");
    forHelp();
}

static Void local forHelp() {
    printf("Type :? for help\n");
}

/* --------------------------------------------------------------------------
 * Setting of command line options:
 * ------------------------------------------------------------------------*/

struct options toggle[] = {		/* List of command line toggles	   */
    {'s', "Print no. reductions/cells after eval", &showStats},
    {'t', "Print type after evaluation",	   &addType},
    {'f', "Terminate evaluation on first error",   &failOnError},
    {'g', "Print no. cells recovered after gc",	   &gcMessages},
    {'l', "Literate scripts as default",	   &literateScripts},
    {'e', "Warn about errors in literate scripts", &literateErrors},
    {'.', "Print dots to show progress",	   &useDots},
    {'w', "Always show which files loaded",	   &listFiles},
    {'k', "Show kind errors in full",		   &kindExpert},
    {'u', "Use \"show\" to display results",	   &useShow},
    {'i', "Chase imports while loading files",	   &chaseImports},
    {0,   0,					   0}
};

static Void local set() {		/* change command line options from*/
    String s;				/* Hugs command line		   */

    if (s=readFilename()) {
	do {
	    if (!processOption(s)) {
		ERRMSG(0) "Option string must begin with `+' or `-'"
		EEND;
	    }
	} while (s=readFilename());
    }
    else
	optionInfo();
}

/* --------------------------------------------------------------------------
 * Change directory command:
 * ------------------------------------------------------------------------*/

static Void local changeDir() {		/* change directory		   */
    String s = readFilename();
    if (s && chdir(s)) {
	ERRMSG(0) "Unable to change to directory \"%s\"", s
	EEND;
    }
}

/* --------------------------------------------------------------------------
 * Loading project and script files:
 * ------------------------------------------------------------------------*/

static Void local loadProject(s)	/* Load project file		  */
String s; {
    clearProject();
    currProject = s;
    projInput(currProject);
    scriptFile = currProject;
    forgetScriptsFrom(1);
    while (s=readFilename())
	addScriptName(s,TRUE);
    if (namesUpto<=1) {
	ERRMSG(0) "Empty project file"
	EEND;
    }
    scriptFile    = 0;
    projectLoaded = TRUE;
}

static Void local clearProject() {     /* clear name for current project   */
    if (currProject)
	free(currProject);
    currProject   = 0;
    projectLoaded = FALSE;
}

static Void local addScriptName(s,sch)	/* Add script to list of files	   */
String s;				/* to be read in ...		   */
Bool   sch; {				/* TRUE => requires pathname search*/
    if (namesUpto>=NUM_SCRIPTS) {
	ERRMSG(0) "Too many script files (maximum of %d allowed)",
		  NUM_SCRIPTS
	EEND;
    }
    else
	scriptName[namesUpto++] = strCopy(sch ? findPathname(NULL,s) : s);
}

static Bool local addScript(fname,len) /* read single script file	   */
String fname;			       /* name of script file		   */
Long   len; {			       /* length of script file 	   */
    scriptFile = fname;

    printf("Reading script file \"%s\":\n",fname);
    setLastEdit(fname,0);

    needsImports = FALSE;
    parseScript(fname,len);	       /* process script file		   */
    if (needsImports)
	return FALSE;
    checkDefns();
    typeCheckDefns();
    compileDefns();
    scriptFile = 0;
    return TRUE;
}

Bool chase(imps)			/* Process list of import requests */
List imps; {
    if (chaseImports) {
	Int    origPos  = numScripts;	/* keep track of original position */
	String origName = scriptName[origPos];
	for (; nonNull(imps); imps=tl(imps)) {
	    String iname = findPathname(origName,textToStr(textOf(hd(imps))));
	    Int    i     = 1;
	    for (; i<namesUpto; i++)
		if (strcmp(scriptName[i],iname)==0)
		    break;
	    if (i>=origPos) {		/* Neither loaded or queued	   */
		String theName;
		Time   theTime;
		Bool   thePost;

		postponed[origPos] = TRUE;
		needsImports       = TRUE;

		if (i>=namesUpto)	/* Name not found (i==namesUpto)   */
		    addScriptName(iname,FALSE);
		else if (postponed[i]) {/* Check for recursive dependency  */
		    ERRMSG(0)
		      "Recursive import dependency between \"%s\" and \"%s\"",
		      scriptName[origPos], iname
		    EEND;
		}
		/* Right rotate section of tables between numScripts and i so
		 * that i ends up with other imports in front of orig. script
		 */
		theName = scriptName[i];
		thePost = postponed[i];
		timeSet(theTime,lastChange[i]);
		for (; i>numScripts; i--) {
		    scriptName[i] = scriptName[i-1];
		    postponed[i]  = postponed[i-1];
		    timeSet(lastChange[i],lastChange[i-1]);
		}
		scriptName[numScripts] = theName;
		postponed[numScripts]  = thePost;
		timeSet(lastChange[numScripts],theTime);
		origPos++;
	    }
	}
	return needsImports;
    }
    return FALSE;
}

static Void local forgetScriptsFrom(scno)/* remove scripts from system	   */
Module scno; {
    Module i;
    for (i=scno; i<namesUpto; ++i)
	if (scriptName[i])
	    free(scriptName[i]);
    dropModulesFrom(scno-1);		 /* don't count prelude as module  */
    namesUpto = scno;
    if (numScripts>namesUpto)
	numScripts = scno;
}

/* --------------------------------------------------------------------------
 * Commands for loading and removing script files:
 * ------------------------------------------------------------------------*/

static Void local load() {	       /* read filenames from command line */
    String s;			       /* and add to list of files waiting */
				       /* to be read			   */
    while (s=readFilename())
	addScriptName(s,TRUE);
    readScripts(1);
}

static Void local project() {	       /* read list of file names from     */
    String s;			       /* project file			   */

    if ((s=readFilename()) || currProject) {
	if (!s)
	    s = strCopy(currProject);
	else if (readFilename()) {
	    ERRMSG(0) "Too many project files"
	    EEND;
	}
	else
	    s = strCopy(s);
    }
    else {
	ERRMSG(0) "No project filename specified"
	EEND;
    }
    loadProject(s);
    readScripts(1);
}

static Void local readScripts(n)	/* Reread current list of scripts, */
Int n; {				/* loading everything after and	   */
    Time timeStamp;			/* including the first script which*/
    Long fileSize;			/* has been either changed or added*/

    for (; n<numScripts; n++) {		/* Scan previously loaded scripts  */
	getFileInfo(scriptName[n], &timeStamp, &fileSize);
	if (timeChanged(timeStamp,lastChange[n])) {
	    dropModulesFrom(n-1);
	    numScripts = n;
	    break;
	}
    }
    for (; n<NUM_SCRIPTS; n++)		/* No files have been postponed at */
	postponed[n] = FALSE;		/* this stage			   */

    while (numScripts<namesUpto) {	/* Process any remaining scripts   */
	getFileInfo(scriptName[numScripts], &timeStamp, &fileSize);
	timeSet(lastChange[numScripts],timeStamp);
	if (numScripts>0)		/* no new module for prelude	   */
	    startNewModule();
	if (addScript(scriptName[numScripts],fileSize))
	    numScripts++;
	else
	    dropModulesFrom(numScripts-1);
    }

    if (listFiles)
	whatFiles();
    if (numScripts<=1)
	setLastEdit((String)0, 0);
}

static Void local whatFiles() {		/* list files in current session   */
    int i;
    printf("\nHugs session for:");
    if (projectLoaded)
	printf(" (project: %s)",currProject);
    for (i=0; i<numScripts; ++i)
	printf("\n%s",scriptName[i]);
    putchar('\n');
}

/* --------------------------------------------------------------------------
 * Access to external editor:
 * ------------------------------------------------------------------------*/

static Void local editor() {		/* interpreter-editor interface	   */
    String newFile  = readFilename();
    if (newFile) {
	setLastEdit(newFile,0);
	if (readFilename()) {
	    ERRMSG(0) "Multiple filenames not permitted"
	    EEND;
	}
    }
    runEditor();
}

static Void local find() {		/* edit file containing definition */
    String nm = readFilename();		/* of specified name		   */
    if (!nm) {
	ERRMSG(0) "No name specified"
	EEND;
    }
    else if (readFilename()) {
	ERRMSG(0) "Multiple names not permitted"
	EEND;
    }
    else {
	Text t;
	Cell c;
	startNewModule();
	if (nonNull(c=findTycon(t=findText(nm))))
	    setLastEdit(scriptName[moduleThisTycon(c)],tycon(c).line);
	else if (nonNull(c=findName(t)))
	    setLastEdit(scriptName[moduleThisName(c)],name(c).line);
	else {
	    ERRMSG(0) "No current definition for name \"%s\"", nm
	    EEND;
	}
	runEditor();
    }
}

static Void local runEditor() {		/* run editor on file lastEdit at  */
    if (startEdit(lastLine,lastEdit))	/* line lastLine		   */
	readScripts(1);
}

static Void local setLastEdit(fname,line)/* keep name of last file to edit */
String fname;
Int    line; {
    if (lastEdit)
	free(lastEdit);
    lastEdit = strCopy(fname);
    lastLine = line;
}

/* --------------------------------------------------------------------------
 * Read and evaluate an expression:
 * ------------------------------------------------------------------------*/

static Void local evaluator() {        /* evaluate expr and print value    */
    Type type, mt;

    scriptFile = 0;
    startNewModule();		       /* Enables recovery of storage	   */
				       /* allocated during evaluation	   */
    parseExp();
    checkExp();
    defaultDefns = evalDefaults;
    type         = typeCheckExp(TRUE);
    mt           = isPolyType(type) ? monoTypeOf(type) : type;
    if (whatIs(mt)==QUAL) {
	ERRMSG(0) "Unresolved overloading" ETHEN
	ERRTEXT   "\n*** type       : "    ETHEN ERRTYPE(type);
	ERRTEXT   "\n*** expression : "    ETHEN ERREXPR(inputExpr);
	ERRTEXT   "\n"
	EEND;
    }
  
#if PROFILING
    profilerLog("profile.hp");
    numReductions = 0;
    garbageCollect();
#endif

#ifdef WANT_TIMER
    updateTimers();
#endif
#if IO_MONAD
    if (typeMatches(type,typeProgIO)) {
	evalExp();
	consGC = FALSE;
	ioExecute();
    }
    else
#endif
    {	Cell printer = namePrint;
	if (useShow) {
	    Cell d = getDictFor(classShow,type);
	    if (isNull(d)) {
		printing = FALSE;
		ERRMSG(0) "Cannot find \"show\" function for:" ETHEN
		ERRTEXT   "\n*** expression : "   ETHEN ERREXPR(inputExpr);
		ERRTEXT   "\n*** of type    : "   ETHEN ERRTYPE(type);
		ERRTEXT   "\n"
		EEND;
	    }
	    printer = dictGet(d,mfunOf(nameShowsPrec));
	}

	evalExp();
	top()  = ap(ap(ap(printer,mkInt(MIN_PREC)),top()),nameNil);
	consGC = FALSE;
	if (addType) {
	    onto(NIL);
	    pushed(0) = pushed(1);
	    pushed(1) = type;
	    outputString(stdout);
	    printf(" :: ");
	    printType(stdout,pop());
	}
	else
	    outputString(stdout);
    }
    stopAnyPrinting();
}

static Void local evalExp() {		/* Compile expression and prepare  */
    compileExp();			/* for execution		   */
    clearStack();
    graphForExp();
    numCells	  = 0;
    numReductions = 0;
    numberGcs     = 0;
    printing      = TRUE;
}

static Void local stopAnyPrinting() {  /* terminate printing of expression,*/
    if (printing) {		       /* after successful termination or  */
	printing = FALSE;	       /* runtime error (e.g. interrupt)   */
	putchar('\n');
	if (showStats) {
#define plural(v)   v, (v==1?"":"s")
	    printf("(%lu reduction%s, ",plural(numReductions));
	    printf("%lu cell%s",plural(numCells));
	    if (numberGcs>0)
		printf(", %u garbage collection%s",plural(numberGcs));
	    printf(")\n");
#undef plural
	}
	fflush(stdout);
    }
}

/* --------------------------------------------------------------------------
 * Print type of input expression:
 * ------------------------------------------------------------------------*/

static Void local showtype() {	       /* print type of expression (if any)*/
    Cell type;

    startNewModule();		       /* Enables recovery of storage	   */
				       /* allocated during evaluation	   */
    parseExp();
    checkExp();
    defaultDefns = evalDefaults;
    type = typeCheckExp(FALSE);
    printExp(stdout,inputExpr);
    printf(" :: ");
    printType(stdout,type);
    putchar('\n');
}

/* --------------------------------------------------------------------------
 * Enhanced help system:  print current list of scripts or give information
 * about an object.
 * ------------------------------------------------------------------------*/

static Void local info() {		/* describe objects		   */
    Int    count = 0;			/* or give menu of commands	   */
    String s;

    startNewModule();			/* for recovery of storage	   */
    for (; s=readFilename(); count++)
	describe(findText(s));
    if (count == 0)
	whatFiles();
}

static Void local describe(t)		/* describe an object		   */
Text t; {
    Tycon tc = findTycon(t);
    Class cl = findClass(t);
    Name  nm = findName(t);

    if (nonNull(tc)) {			/* as a type constructor	   */
	Type t = tc;
	Int  i;
	Inst in;
	for (i=0; i<tycon(tc).arity; ++i)
	    t = ap(t,mkOffset(i));
	printf("-- type constructor");
	if (kindExpert) {
	    printf(" with kind ");
	    printKind(stdout,tycon(tc).kind);
	}
	putchar('\n');
	switch (tycon(tc).what) {
	    case SYNONYM      : printf("type ");
				printType(stdout,t);
				printf(" = ");
				printType(stdout,tycon(tc).defn);
				break;

	    case NEWTYPE      :
	    case DATATYPE     : {   List cs = tycon(tc).defn;
				    if (tycon(tc).what==DATATYPE)
					printf("data ");
				    else
					printf("newtype ");
				    printType(stdout,t);
				    if (hasCfun(cs))
					printf("\n\n-- constructors:");
				    for (; hasCfun(cs); cs=tl(cs)) {
					putchar('\n');
					printExp(stdout,hd(cs));
					printf(" :: ");
					printType(stdout,name(hd(cs)).type);
				    }
				    if (nonNull(cs))
					printf("\n\n-- selectors:");
				    for (; nonNull(cs); cs=tl(cs)) {
					putchar('\n');
					printExp(stdout,hd(cs));
					printf(" :: ");
					printType(stdout,name(hd(cs)).type);
				    }
				}
				break;

	    case RESTRICTSYN  : printf("type ");
				printType(stdout,t);
				printf(" = <restricted>");
				break;
	}
	putchar('\n');
	if (nonNull(in=findFirstInst(tc))) {
	    printf("\n-- instances:\n");
	    do {
		showInst(in);
		in = findNextInst(tc,in);
	    } while (nonNull(in));
	}
	putchar('\n');
    }

    if (nonNull(cl)) {			/* as a class			   */
	List ins = cclass(cl).instances;
	if (cclass(cl).sig==STAR)
	    printf("-- type class");
	else {
	    printf("-- constructor class");
	    if (kindExpert) {
		printf(" with instances of kind ");
		printKind(stdout,cclass(cl).sig);
	    }
	}
	printf("\nclass ");
	if (nonNull(cclass(cl).supers)) {
	    List cs = cclass(cl).supers;
	    if (nonNull(tl(cs))) {
		putchar('(');
		for (;;) {
		    printf("%s a",textToStr(cclass(hd(cs)).text));
		    if (nonNull(cs=tl(cs)))
			printf(", ");
		    else
			break;
		}
		putchar(')');
	    }
	    else
		printf("%s a",textToStr(cclass(hd(cs)).text));

	    printf(" => ");
	}
	printf("%s a",textToStr(cclass(cl).text));
	if (nonNull(cclass(cl).members)) {
	    List ms = cclass(cl).members;
	    printf(" where");
	    do {
		Type t = monoTypeOf(name(hd(ms)).type);
		printf("\n  ");
		printExp(stdout,hd(ms));
		printf(" :: ");
		if (isNull(tl(fst(snd(t)))))
		    t = snd(snd(t));
		else
		    t = ap(QUAL,pair(tl(fst(snd(t))),snd(snd(t))));
		printType(stdout,t);
		ms = tl(ms);
	    } while (nonNull(ms));
	}
	putchar('\n');
	if (nonNull(ins)) {
	    printf("\n-- instances:\n");
	    do {
		showInst(hd(ins));
		ins = tl(ins);
	    } while (nonNull(ins));
	}
	putchar('\n');
    }

    if (nonNull(nm)) {			/* as a function/name		   */
	printExp(stdout,nm);
	printf(" :: ");
	if (nonNull(name(nm).type)) {	/* we have to run the type checker */
	    inputExpr = nm;		/* to convert GTC type to HTC form */
	    printType(stdout,typeCheckExp(FALSE));
	}
	else
	    printf("<unknown type>");

	if (isCfun(nm))
	    printf("  -- data constructor");
	else if (isMfun(nm))
	    printf("  -- class member");
	else if (isSfun(nm))
	    printf("  -- selector function");

	if (name(nm).primDef)
	    printf("   -- primitive");
	printf("\n\n");
    }

    if (isNull(tc) && isNull(cl) && isNull(nm)) {
	printf("Unknown reference `%s'\n",textToStr(t));
    }
}

static Void local showInst(in)		/* Display instance decl header	   */
Inst in; {
    printf("instance ");
    if (nonNull(inst(in).specifics)) {
	printContext(stdout,inst(in).specifics);
	printf(" => ");
    }
    printPred(stdout,makeInstPred(in));
    putchar('\n');
}

/* --------------------------------------------------------------------------
 * List all names currently in scope:
 * ------------------------------------------------------------------------*/

static Void local listNames() {		/* list names matching optional pat*/
    String pat   = readFilename();
    List   names = NIL;
    Int    width = getTerminalWidth() - 1;
    Int    count = 0;
    Int    termPos;

    if (pat)				/* First gather names to list	   */
	do
	    names = addNamesMatching(pat,names);
	while (pat=readFilename());
    else
	names = addNamesMatching((String)0,names);

    if (isNull(names)) {		/* Then print them out		   */
	ERRMSG(0) "No names selected"
	EEND;
    }
    for (termPos=0; nonNull(names); names=tl(names)) {
	String s = textToStr(name(hd(names)).text);
	Int    l = strlen(s);
	if (termPos+1+l>width) {
	    putchar('\n');
	    termPos = 0;
	}
	else if (termPos>0) {
	    putchar(' ');
	    termPos++;
	}
	printf("%s",s);
	termPos += l;
	count++;
    }
    printf("\n(%d names listed)\n", count);
}

/* --------------------------------------------------------------------------
 * main read-eval-print loop, with error trapping:
 * ------------------------------------------------------------------------*/

static jmp_buf catch_error;	       /* jump buffer for error trapping   */

static Void local interpreter(argc,argv)/* main interpreter loop	   */
Int    argc;
String argv[]; {
    Int errorNumber = setjmp(catch_error);

    breakOn(TRUE);		       /* enable break trapping 	   */
    if (numScripts==0) {	       /* only succeeds on first time,	   */
	if (errorNumber)	       /* before prelude has been loaded   */
	    fatal("Unable to load prelude");
	initialize(argc,argv);
	forHelp();
    }

    for (;;) {
	Command cmd;
	everybody(RESET);		/* reset to sensible initial state */
	dropModulesFrom(numScripts-1);	/* remove partially loaded scripts */
					/* not counting prelude as a module*/
	consoleInput(prompt);
	cmd = readCommand(cmds, (Char)':', (Char)'!');
#ifdef WANT_TIMER
	updateTimers();
#endif
	switch (cmd) {
	    case EDIT	: editor();
			  break;
	    case FIND   : find();
			  break;
	    case LOAD	: clearProject();
			  forgetScriptsFrom(1);
			  load();
			  break;
	    case ALSO   : clearProject();
			  forgetScriptsFrom(numScripts);
			  load();
			  break;
	    case RELOAD : readScripts(1);
			  break;
	    case PROJECT: project();
			  break;
	    case EVAL	: evaluator();
			  break;
	    case TYPEOF : showtype();
			  break;
	    case NAMES  : listNames();
			  break;
	    case HELP	: menu();
			  break;
	    case BADCMD : guidance();
			  break;
	    case SET    : set();
			  break;
	    case SYSTEM : shellEsc(readLine());
			  break;
	    case CHGDIR : changeDir();
			  break;
	    case INFO   : info();
			  break;
	    case QUIT	: return;
	    case COLLECT: consGC = FALSE;
			  garbageCollect();
			  consGC = TRUE;
			  printf("Garbage collection recovered %d cells\n",
				 cellsRecovered);
			  break;
	    case NOCMD	: break;
	}
#ifdef WANT_TIMER
	updateTimers();
	printf("Elapsed time (ms): %ld (user), %ld (system)\n",
	       millisecs(userElapsed), millisecs(systElapsed));
#endif
    }
}

/* --------------------------------------------------------------------------
 * Display progress towards goal:
 * ------------------------------------------------------------------------*/

static Target currTarget;
static Bool   aiming = FALSE;
static Int    currPos;
static Int    maxPos;
static Int    charCount;

Void setGoal(what, t)		       /* Set goal for what to be t	   */
String what;
Target t; {
    currTarget = (t?t:1);
    aiming     = TRUE;
    if (useDots) {
	currPos = strlen(what);
	maxPos  = getTerminalWidth() - 1;
	printf("%s",what);
    }
    else
	for (charCount=0; *what; charCount++)
	    putchar(*what++);
    fflush(stdout);
}

Void soFar(t)			       /* Indicate progress towards goal   */
Target t; {			       /* has now reached t		   */
    if (useDots) {
	Int newPos = (Int)((maxPos * ((long)t))/currTarget);

	if (newPos>maxPos)
	    newPos = maxPos;

	if (newPos>currPos) {
	    do
		putchar('.');
	    while (newPos>++currPos);
	    fflush(stdout);
	}
	fflush(stdout);
    }
}

Void done() {			       /* Goal has now been achieved	   */
    if (useDots) {
	while (maxPos>currPos++)
	    putchar('.');
	putchar('\n');
	aiming = FALSE;
    }
    else
	for (; charCount>0; charCount--) {
	    putchar('\b');
	    putchar(' ');
	    putchar('\b');
	}
    fflush(stdout);
}

static Void local failed() {	       /* Goal cannot be reached due to    */
    if (aiming) {		       /* errors			   */
	aiming = FALSE;
	putchar('\n');
	fflush(stdout);
    }
}

/* --------------------------------------------------------------------------
 * Error handling:
 * ------------------------------------------------------------------------*/

Void errHead(l) 		       /* print start of error message	   */
Int l; {
    failed();			       /* failed to reach target ...	   */
    stopAnyPrinting();
    fprintf(errorStream,"ERROR");

    if (scriptFile) {
	fprintf(errorStream," \"%s\"", scriptFile);
	setLastEdit(scriptFile,l);
	if (l) fprintf(errorStream," (line %d)",l);
	scriptFile = 0;
    }
    fprintf(errorStream,": ");
    fflush(errorStream);
}

Void errFail() {			/* terminate error message and	   */
    putc('\n',errorStream);		/* produce exception to return to  */
    fflush(errorStream);		/* main command loop		   */
    longjmp(catch_error,1);
}

Void errAbort() {			/* altern. form of error handling  */
    failed();				/* used when suitable error message*/
    stopAnyPrinting();			/* has already been printed	   */
    errFail();
}

Void internal(msg)			/* handle internal error 	   */
String msg; {
    failed();
    stopAnyPrinting();
    fprintf(errorStream,"INTERNAL ERROR: %s\n",msg);
    fflush(errorStream);
    longjmp(catch_error,1);
}

Void fatal(msg)				/* handle fatal error		   */
String msg; {
    fflush(stdout);
    printf("\nFATAL ERROR: %s\n",msg);
    everybody(EXIT);
    exit(1);
}

sigHandler(breakHandler) {		/* respond to break interrupt	   */
    Hilite
    printf("{Interrupted!}\n");
    Lolite
    breakOn(TRUE);
    everybody(BREAK);
    failed();
    stopAnyPrinting();
    fflush(stdout);
    longjmp(catch_error,1);
    sigResume;/*NOTREACHED*/
}

/* --------------------------------------------------------------------------
 * Read value from environment variable:
 * ------------------------------------------------------------------------*/

String fromEnv(var,def)		/* return value of:	 		   */
String var;			/*     environment variable named by var   */
String def; {			/* or: default value given by def	   */
    String s = getenv(var);

    return (s ? s : def);
}

/* --------------------------------------------------------------------------
 * String manipulation routines:
 * ------------------------------------------------------------------------*/

static String local strCopy(s)	       /* make malloced copy of a string   */
String s; {
    if (s && *s) {
	char *t, *r;
	if ((t=(char *)malloc(strlen(s)+1))==0) {
	    ERRMSG(0) "String storage space exhausted"
	    EEND;
	}
	for (r=t; *r++ = *s++; )
	    ;
	return t;
    }
    return NULL;
}

/* --------------------------------------------------------------------------
 * Send message to each component of system:
 * ------------------------------------------------------------------------*/

Void everybody(what)		/* send command `what' to each component of*/
Int what; {			/* system to respond as appropriate ...    */
    machdep(what);		/* The order of calling each component is  */
    storage(what);		/* important for the INSTALL command	   */
    input(what);
    staticAnalysis(what);
    typeChecker(what);
    compiler(what);
    machine(what);
    builtIn(what);
}

/*-------------------------------------------------------------------------*/
