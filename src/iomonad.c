/* --------------------------------------------------------------------------
 * iomonad.c:   Copyright (c) Mark P Jones 1991-1996.   All rights reserved.
 *              See NOTICE for details and conditions of use etc...
 *              Hugs version 1.3, August 1996
 *
 * Implementation of Haskell IO monad.
 *
 * The primitives below implement the standard IO monad for Haskell 1.3
 * using a continuation passing bimonad (two streams of processing for
 * `exceptional' and `normal' I/O respectively).  The primitives are
 * believed to give a reasonably good implementation of the semantics
 * specified by the Haskell 1.3 report.  There are also some additional
 * primitives, particularly for dealing with IOError and Handle values
 * that are not included in the prelude, but have been suggested for
 * inclusion in standard libraries.  I don't know what semantics and
 * specifications will be specified for these operations in the final
 * version of the 1.3 I/O specification, so be prepared for changes.
 * ------------------------------------------------------------------------*/
 
Void ioExecute() {			/* execute IO program of type IO() */
    Cell temp;
    noechoTerminal();
    toparg(nameUncaught);
    toparg(nameDone);
    temp = evalWithNoError(pop());
    if (nonNull(temp)) {
	push(temp);
	abandon("Program execution",top());
    }
}

primFun(primUncaught) {			/* uncaught error continuation	   */
    eval(top());			/* try to decode IOError value	   */
    out = NIL;				/* ... most leave a string on stack*/
    outCh('\n');
    if (whnfHead==nameUserErr)		/* test for various err conditions */
	outStr("User error: ");
    else if (whnfHead==nameNameErr)
	outStr("Illegal file name: ");
    else if (whnfHead==nameSearchErr)
	outStr("File not found: ");
    else if (whnfHead==nameWriteErr)
	outStr("Cannot write to file: ");
    else if (whnfHead==nameEvalErr)
	outStr("Evaluator error: ");
    else if (whnfHead==nameIllegal) {
	outStr("Illegal operation");
	push(nameNil);
    }
    else {
	outStr("Uncaught I/O exception!");
	push(nameNil);
    }

    top() = revOnto(out,top());
    out   = NIL;
    outputString(errorStream);
    errAbort();
}

primFun(primDone) {			/* terminating continuation	   */
    updateRoot(nameUnit);		/* most anything would do here	   */
}

primFun(primLunit) {			/* bimonad left unit		   */
    updapRoot(primArg(2),primArg(3));	/* lunit 3 2 1 = 2 3		   */
}

primFun(primRunit) {			/* bimonad right unit		   */
    updapRoot(primArg(1),primArg(3));	/* lunit 3 2 1 = 1 3		   */
}

primFun(primLbind) {			/* bimonad left bind		   */
    push(ap(namePass,primArg(3)));	/* lbind 4 3 2 1 = 4 (pass 3 2 1) 1*/
    toparg(primArg(2));
    toparg(primArg(1));
    updapRoot(ap(primArg(4),top()),primArg(1));
}

primFun(primRbind) {			/* bimonad right bind		   */
    push(ap(namePass,primArg(3)));	/* rbind 4 3 2 1 = 4 2 (pass 3 2 1)*/
    toparg(primArg(2));
    toparg(primArg(1));
    updapRoot(ap(primArg(4),primArg(2)),top());
}

primFun(primPass) {			/* Auxiliary function		   */
    push(ap(primArg(4),primArg(1)));	/* pass 4 3 2 1 = 4 1 3 2	   */
    toparg(primArg(3));
    updapRoot(top(),primArg(2));
}

primFun(primGetCh) {			/* Get character from stdin wo/echo*/
    updapRoot(primArg(1),mkChar(readTerminalChar()));
}

primFun(primGetChar) {			/* Get character from stdin w/ echo*/
    Char c = readTerminalChar();
    putchar(c);
    fflush(stdout);
    updapRoot(primArg(1),mkChar(c));
}

primFun(primHGetChar) {			/* Read character from handle	   */
    Int h = 0;
    eval(pop());
    h     = intValOf(whnfHead);
    if (handles[h].hmode&HREAD) {
	Char c = (h==HSTDIN ? readTerminalChar() : getc(handles[h].hfp));
	if (c!=EOF) {
	    updapRoot(primArg(1),mkChar(c));
	    return;
	}
    }
    updapRoot(primArg(2),nameIllegal);
}

primFun(primPutChar) {			/* print character on stdout	   */
    eval(pop());
    putchar(charOf(whnfHead));
    fflush(stdout);
    updapRoot(primArg(1),nameUnit);
}

primFun(primHPutChar) {			/* print character on handle	   */
    Int  h = 0;
    eval(pop());
    h      = intValOf(whnfHead);
    if (handles[h].hmode&(HWRITE|HAPPEND)) {
	eval(pop());
	putc(charOf(whnfHead),handles[h].hfp);
	updapRoot(primArg(1),nameUnit);
	return;
    }
    updapRoot(primArg(2),nameIllegal);
}

primFun(primPutStr) {			/* print string on stdout	   */
    updapRoot(nameJust,nameUnit);	/* supposedly = hPutStr stdout,	   */
    eval(pop());			/* included here for speed	   */
    while (whnfHead==nameCons) {
	eval(pop());
	putchar(charOf(whnfHead));
#if FLUSHEVERY
	fflush(stdout);
#endif
	eval(pop());
    }
#if !FLUSHEVERY
    fflush(stdout);
#endif
    updapRoot(primArg(1),nameUnit);
}

primFun(primHPutStr) {			/* print string on handle	   */
    Int  h = 0;
    eval(pop());
    h      = intValOf(whnfHead);
    if (handles[h].hmode&(HWRITE|HAPPEND)) {
	updapRoot(nameJust,nameUnit);
	eval(pop());
	while (whnfHead==nameCons) {
	    eval(pop());
	    putc(charOf(whnfHead),handles[h].hfp);
#if FLUSHEVERY
	    fflush(handles[h].hfp);
#endif
	    eval(pop());
	}
#if !FLUSHEVERY
	fflush(handles[h].hfp);
#endif
	updapRoot(primArg(1),nameUnit);
	return;
    }
    updapRoot(primArg(2),nameIllegal);
}

static Name nameHreader;	        /* auxiliary function		   */

primFun(primHreader) {			/* read String from a handle 	   */
    Int h=0;
    eval(primArg(1));			/* evaluate handle		   */
    h = intValOf(whnfHead);
    if (handles[h].hmode&HSEMICLOSED) {	/* read requires semi-closed handle*/
	Int c = (h==HSTDIN ? readTerminalChar() : getc(handles[h].hfp));
	if (c!=EOF && c>=0 && c<NUM_CHARS) {
	    updapRoot(consChar(c),ap(nameHreader,primArg(1)));
	    return;
	}
	clearerr(handles[h].hfp);
    }
    updateRoot(nameNil);
}

primFun(primHContents) {		/* hGetContents :: Handle -> IO Str*/
    Int h = 0;
    eval(primArg(3));
    h = intValOf(whnfHead);
    if ((handles[h].hmode&HREAD)==0)	/* must have readable handle	   */
	updapRoot(primArg(2),nameIllegal);
    else {				/* semi-close handle		   */
	handles[h].hmode = HSEMICLOSED;
	updapRoot(primArg(1),ap(nameHreader,primArg(3)));
    }
}

primFun(primContents) {			/* Get contents of stdin	   */
    if ((handles[HSTDIN].hmode&HREAD)==0)
	updapRoot(primArg(2),nameIllegal);
    else {
	handles[HSTDIN].hmode = HSEMICLOSED;
	updapRoot(primArg(1),ap(nameHreader,handles[HSTDIN].hcell));
    }
}

primFun(primOpenFile) {			/* open handle to a file	   */
    String s = evalName(primArg(4));	/* Eval and check filename	   */
    Int    m = HCLOSED;

    if (!s)				/* check for valid name		   */
	updapRoot(primArg(2),ap(nameNameErr,primArg(4)));

    eval(primArg(3));			/* Eval IOMode			   */
    if (isName(whnfHead) && isCfun(whnfHead))
	switch (cfunOf(whnfHead)) {	/* we have to use numeric consts   */
	    case 1 : m = HREAD;		/* here to avoid the need to put   */
		     break;		/* IOMode in startup environment   */
	    case 2 : m = HWRITE;
		     break;
	    case 3 : m = HAPPEND;
		     break;
	}

    if (m!=HCLOSED) {			/* Only accept legal modes	   */
	Cell hnd = openHandle(s,m);
	if (nonNull(hnd)) {
	    updapRoot(primArg(1),hnd);
	    return;
	}
    }

    updapRoot(primArg(2),nameIllegal);
}

primFun(primStdin) {			/* Standard input handle	   */
    push(handles[HSTDIN].hcell);
}

primFun(primStdout) {			/* Standard output handle	   */
    push(handles[HSTDOUT].hcell);
}

primFun(primStderr) {			/* Standard error handle	   */
    push(handles[HSTDERR].hcell);
}

primFun(primHIsEOF) {			/* Test for end of file on handle  */
    Int h = 0;
    eval(primArg(3));
    h     = intValOf(whnfHead);
    if (handles[h].hmode!=HCLOSED)
	updapRoot(primArg(1),(feof(handles[h].hfp) ? nameTrue : nameFalse));
    else
	updapRoot(primArg(2),nameIllegal);
}

primFun(primHFlush) {			/* Flush handle			   */
    Int h = 0;
    eval(primArg(3));
    h     = intValOf(whnfHead);
    if (handles[h].hmode!=HCLOSED) {
	fflush(handles[h].hfp);
	updapRoot(primArg(1),nameUnit);
    }
    else
	updapRoot(primArg(2),nameIllegal);
}

primFun(primHClose) {			/* Close handle			   */
    Int h = 0;
    eval(primArg(3));
    h     = intValOf(whnfHead);
    if (handles[h].hmode!=HCLOSED) {
	if (h>HSTDERR && handles[h].hfp)
	    fclose(handles[h].hfp);
	handles[h].hfp   = 0;
	handles[h].hmode = HCLOSED;
	updapRoot(primArg(1),nameUnit);
    }
    else
	updapRoot(primArg(2),nameIllegal);
}

primFun(primReadFile) {			/* read file as lazy string	   */
    String s   = evalName(primArg(3));	/* Eval and check filename	   */
    Cell   hnd = NIL;
    if (!s)
	updapRoot(primArg(2),ap(nameNameErr,primArg(3)));
    else if (isNull(hnd=openHandle(s,HREAD)))
	updapRoot(primArg(2),ap(nameSearchErr,primArg(3)));
    else {
	handles[intValOf(hnd)].hmode = HSEMICLOSED;
	updapRoot(primArg(1),ap(nameHreader,hnd));
    }
}

primFun(primWriteFile) {		/* write string to specified file  */
    fwritePrim(root,FALSE);
}

primFun(primAppendFile) {		/* append string to specified file */
    fwritePrim(root,TRUE);
}

static Void local fwritePrim(root, append)	/* Auxiliary function for  */
StackPtr root;					/* writing/appending to	   */
Bool     append; {				/* an output file	   */
    String mode = append ? FOPEN_APPEND : FOPEN_WRITE;
    String s    = evalName(primArg(4));		/* Eval and check filename */
    if (!s)
	updapRoot(primArg(2),ap(nameNameErr,primArg(4)));
    else if (append && access(s,0)!=0)		/* Check that file exists  */
	updapRoot(primArg(2),ap(nameSearchErr,primArg(4)));
    else if ((writingFile=fopen(s,mode))==0)	/* Open file for writing   */
	updapRoot(primArg(2),ap(nameWriteErr,primArg(4)));
    else {					/* Output characters	   */
	Cell temp = NIL;
	updapRoot(nameJust,nameUnit);
	drop();
	eval(pop());
	while (whnfHead==nameCons) {
	    eval(pop());
	    fputc(charOf(whnfHead),writingFile);
	    eval(pop());
	}
	fclose(writingFile);
	writingFile = 0;
	if (nonNull(temp)) {
	    push(temp);
	    top() = printDBadRedex(temp,nameNil);
	    updapRoot(primArg(2),ap(nameEvalErr,top()));
	}
	else
	    updapRoot(primArg(1),nameUnit);
    }
}

primFun(primUserError) {		/* :: String -> IOError		   */
    updapRoot(nameUserErr,primArg(1));
}

primFun(primIsUserErr) {		/* :: IOError -> Maybe String	   */
    eval(primArg(1));
    if (whnfHead==nameUserErr)
	updapRoot(nameJust,top());
    else
	updateRoot(nameNothing);
}

primFun(primIsIllegal) {		/* :: IOError -> Bool		   */
    eval(primArg(1));
    updateRoot((whnfHead==nameIllegal)?nameTrue:nameFalse);
}

primFun(primIsUnsupported) {		/* :: IOError -> Bool		   */
    updateRoot(nameFalse);
}

primFun(primGetHandle) {		/* :: IOError -> Maybe Handle	   */
    eval(primArg(1));
    /* insert tests here */
    updateRoot(nameNothing);
}

primFun(primGetFileName) {		/* :: IOError -> Maybe FilePath	   */
    eval(primArg(1));
    if (whnfHead==nameNameErr || whnfHead==nameSearchErr
			      || whnfHead==nameWriteErr)
	updapRoot(nameJust,top());
    else
	updateRoot(nameNothing);
}

#if IO_REFS
primFun(primNewRef) {			/* a -> IO (Ref a)		   */
    updapRoot(primArg(1),ap(MUTVAR,primArg(3)));
}

primFun(primDerefRef) {			/* Ref a -> IO a		   */
    eval(pop());
    updapRoot(primArg(1),snd(whnfHead));
}

primFun(primAssignRef) {		/* Ref a -> a -> IO ()		   */
    eval(primArg(4));
    snd(whnfHead) = primArg(3);
    updapRoot(primArg(1),nameUnit);
}

primFun(primEqRef) {			/* Ref a -> Ref a -> Bool	   */
    Cell x;
    eval(pop());
    x = whnfHead;
    eval(pop());
    updateRoot(x==whnfHead ? nameTrue : nameFalse);
}
#endif

/*-------------------------------------------------------------------------*/
