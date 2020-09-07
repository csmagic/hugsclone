/* --------------------------------------------------------------------------
 * parser.y:    Copyright (c) Mark P Jones 1991-1996.   All rights reserved.
 *              See NOTICE for details and conditions of use etc...
 *              Hugs version 1.3, August 1996
 *
 *              Expect 20 shift/reduce conflicts when passing this grammar
 *              through yacc, but don't worry; they will all be resolved in
 *              an appropriate manner.
 *
 * Hugs parser (included as part of input.c)
 * ------------------------------------------------------------------------*/

%{
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

%}

%token EXPR       SCRIPT
%token CASEXP     OF         DATA       TYPE       IF
%token THEN       ELSE       WHERE      LET        IN
%token INFIX      INFIXL     INFIXR     PRIMITIVE  TNEWTYPE
%token DEFAULT    DERIVING   DO         TCLASS     TINSTANCE
%token TRUNST     REPEAT
%token VAROP      VARID      NUMLIT     CHARLIT    STRINGLIT
%token CONOP      CONID
%token COCO       '='        UPTO       '@'        '\\'
%token '|'        '-'        FROM       ARROW      '~'
%token '!'        IMPLIES    '('        ','        ')'
%token '['        ';'        ']'        '`'
%token MODULE     IMPORT     HIDING     QUALIFIED  ASMOD

%%
/*- Top level script/module structure -------------------------------------*/

start     : EXPR exp wherePart          {inputExpr = letrec($3,$2); sp-=2;}
	  | SCRIPT topModule            {valDefns  = $2;            sp-=1;}
	  | error                       {syntaxError("input");}
	  ;

/*- Haskell module header/import parsing: -----------------------------------
 * Syntax for Haskell modules (module headers and imports) is parsed but
 * most of it is ignored.  However, module names in import declarations
 * are used, of course, if import chasing is turned on.
 *-------------------------------------------------------------------------*/

topModule : begin modBody end           {$$ = gc2($2);}
	  | modules                     {$$ = $1;}
	  ;
modules   : modules module              {$$ = gc2(appendOnto($2,$1));}
	  | module                      {$$ = $1;}
	  ;
module    : MODULE modid expspec WHERE '{' modBody end
					{$$ = gc7($6);}
	  | MODULE error                {syntaxError("module definition");}
	  ;
modid     : CONID			{$$ = $1;}
	  | VARID			{$$ = $1;}
	  | STRINGLIT			{$$ = $1;}
	  ;
modBody   : topDecls			{$$ = $1;}
	  | fixDecls ';' topDecls	{$$ = gc3($3);}
	  | impDecls chase		{$$ = gc2(NIL);}
	  | impDecls ';' chase topDecls	{$$ = gc4($4);}
	  | impDecls ';' chase fixDecls ';' topDecls
					{$$ = gc6($6);}
	  ;

/*- Exports: --------------------------------------------------------------*/

expspec   : /* empty */                 {$$ = gc0(NIL);}
	  | '(' exports ')'             {$$ = gc3(NIL);}
	  | '(' exports ',' ')'         {$$ = gc4(NIL);}
	  ;
exports   : exports ',' export          {$$ = gc3(NIL);}
	  | export                      {$$ = $1;}
	  ;
export    : import                      {$$ = $1;}
	  | MODULE modid                {$$ = gc2(NIL);}
	  ;

/*- Import declarations: --------------------------------------------------*/

impDecls  : impDecls ';' impDecl	{imps = cons($3,imps); $$=gc3(NIL);}
	  | impDecl			{imps = singleton($1); $$=gc1(NIL);}
	  ;
chase	  : /* empty */			{if (chase(imps)) {
					     clearStack();
					     onto(imps);
                                             done();
					     closeAnyInput();
					     return 0;
					 }
					 $$ = gc0(NIL);
					}
	  ;
impDecl   : IMPORT modid impspec        {$$ = gc3($2);}
	  | IMPORT QUALIFIED modid ASMOD modid impspec
					{$$ = gc6($3);}
	  | IMPORT QUALIFIED modid impspec
					{$$ = gc4($3);}
	  | IMPORT error                {syntaxError("import declaration");}
	  ;
impspec   : /* empty */                 {$$ = gc0(NIL);}
	  | HIDING '(' imports ')'      {$$ = gc4(NIL);}
	  | '(' imports ')'             {$$ = gc3(NIL);}
	  ;
imports   : /* empty */                 {$$ = gc0(NIL);}
	  | ','                         {$$ = gc1(NIL);}
	  | imports1                    {$$ = $1;}
	  | imports1 ','                {$$ = gc2($1);}
	  ;
imports1  : imports1 ',' import         {$$ = gc3(NIL);}
	  | import                      {$$ = $1;}
	  ;
import    : var                         {$$ = $1;}
	  | CONID                       {$$ = $1;}
	  | CONID '(' UPTO ')'          {$$ = gc4(NIL);}
	  | CONID '(' cnames ')'        {$$ = gc4(NIL);}
	  ;
cnames    : /* empty */                 {$$ = gc0(NIL);}
	  | ','                         {$$ = gc1(NIL);}
	  | cnames1                     {$$ = $1;}
	  | cnames1 ','                 {$$ = gc2($1);}
	  ;
cnames1   : cnames1 ',' cname           {$$ = gc3(NIL);}
	  | cname                       {$$ = gc1(NIL);}
	  ;
cname     : var                         {$$ = $1;}
	  | conid                       {$$ = $1;}
	  ;

/*- Fixity declarations: --------------------------------------------------*/

fixDecls  : fixDecls ';' fixDecl        {$$ = gc2(NIL);}
	  | fixDecl                     {$$ = gc0(NIL);}
	  ;
fixDecl   : INFIXL optdigit ops         {fixDefn(LEFT_ASS,$1,$2,$3); sp-=3;}
	  | INFIXR optdigit ops         {fixDefn(RIGHT_ASS,$1,$2,$3);sp-=3;}
	  | INFIX  optdigit ops         {fixDefn(NON_ASS,$1,$2,$3);  sp-=3;}
	  ;
optdigit  : NUMLIT                      {$$ = gc1(checkPrec($1));}
	  | /* empty */                 {$$ = gc0(mkInt(DEF_PREC));}
	  ;
ops       : ops ',' op                  {$$ = gc3(cons($3,$1));}
	  | op                          {$$ = gc1(cons($1,NIL));}
	  ;
op        : varop                       {$$ = $1;}
	  | conop                       {$$ = $1;}
	  | '-'                         {$$ = gc1(varMinus);}
	  ;
varop     : VAROP                       {$$ = $1;}
	  | '!'                         {$$ = gc1(varBang);}
	  | '`' varid1 '`'              {$$ = gc3($2);}
	  ;
conop     : CONOP                       {$$ = $1;}
	  | '`' CONID '`'               {$$ = gc3($2);}
	  ;

/*- Top-level declarations: -----------------------------------------------*/

topDecls  : /* empty */                 {$$ = gc0(NIL);}
	  | ';'                         {$$ = gc1(NIL);}
	  | topDecls1                   {$$ = $1;}
	  | topDecls1 ';'               {$$ = gc2($1);}
	  ;
topDecls1 : topDecls1 ';' topDecl       {$$ = gc2($1);}
	  | topDecls1 ';' decl          {$$ = gc3(cons($3,$1));}
	  | topDecl                     {$$ = gc0(NIL);}
	  | decl                        {$$ = gc1(cons($1,NIL));}
	  ;

/*- Type declarations: ----------------------------------------------------*/

topDecl   : TYPE tyLhs '=' type         {defTycon(4,$3,$2,$4,SYNONYM);}
	  | TYPE tyLhs '=' type IN invars
					{defTycon(6,$3,$2,
						    ap($4,$6),RESTRICTSYN);}
	  | DATA type '=' constrs deriving
					{defTycon(5,$3,checkTyLhs($2),
						    ap(rev($4),$5),DATATYPE);}
	  | DATA context IMPLIES tyLhs '=' constrs deriving
					{defTycon(7,$5,$4,
						  ap(ap(QUAL,pair($2,rev($6))),
						     $7),DATATYPE);}
	  | DATA type                   {defTycon(2,$1,checkTyLhs($2),
						    ap(NIL,NIL),DATATYPE);}
	  | DATA context IMPLIES tyLhs  {defTycon(4,$1,$4,
						  ap(ap(QUAL,pair($2,NIL)),
						     NIL),DATATYPE);}
	  | TNEWTYPE type '=' nconstr deriving
					{defTycon(5,$3,checkTyLhs($2),
						    ap($4,$5),NEWTYPE);}
	  | TNEWTYPE context IMPLIES tyLhs '=' nconstr deriving
					{defTycon(7,$5,$4,
						  ap(ap(QUAL,pair($2,$6)),
						     $7),NEWTYPE);}
	  ;
tyLhs     : tyLhs varid1                {$$ = gc2(ap($1,$2));}
	  | CONID                       {$$ = $1;}
	  | error                       {syntaxError("type defn lhs");}
	  ;
invars    : invars ',' invar            {$$ = gc3(cons($3,$1));}
	  | invar                       {$$ = gc1(cons($1,NIL));}
	  ;
invar     : var COCO sigType            {$$ = gc3(sigdecl($2,singleton($1),
							     $3));}
	  | var                         {$$ = $1;}
	  ;
constrs   : constrs '|' constr          {$$ = gc3(cons($3,$1));}
	  | constr                      {$$ = gc1(cons($1,NIL));}
	  ;
constr    : '!' btype conop bbtype      {$$ = gc4(ap(ap($3,bang($2)),$4));}
	  | btype1    conop bbtype      {$$ = gc3(ap(ap($2,$1),$3));}
	  | btype2    conop bbtype      {$$ = gc3(ap(ap($2,$1),$3));}
	  | btype2                      {$$ = $1;}
	  | btype3                      {$$ = $1;}
	  | conid '{' fieldspecs '}'    {$$ = gc4(ap(LABC,pair($1,rev($3))));}
	  | error                       {syntaxError("data type definition");}
	  ;
btype3    : btype2 '!' atype            {$$ = gc3(ap($1,bang($3)));}
	  | btype3 '!' atype            {$$ = gc3(ap($1,bang($3)));}
	  | btype3 atype                {$$ = gc2(ap($1,$2));}
	  ;
bbtype    : '!' btype                   {$$ = gc2(bang($2));}
	  | btype                       {$$ = $1;}
	  ;
fieldspecs: fieldspecs ',' fieldspec    {$$ = gc3(cons($3,$1));}
	  | fieldspec                   {$$ = gc1(cons($1,NIL));}
	  ;
fieldspec : vars COCO type              {$$ = gc3(pair(rev($1),$3));}
	  | vars COCO '!' type          {$$ = gc4(pair(rev($1),bang($4)));}
	  ;
nconstr   : conid atype                 {$$ = gc2(singleton(ap($1,$2)));}
	  ;
deriving  : /* empty */                 {$$ = gc0(NIL);}
	  | DERIVING CONID              {$$ = gc2(singleton($2));}
	  | DERIVING '(' derivs0 ')'    {$$ = gc4($3);}
	  ;
derivs0   : /* empty */                 {$$ = gc0(NIL);}
	  | derivs                      {$$ = gc1(rev($1));}
	  ;
derivs    : derivs ',' CONID            {$$ = gc3(cons($3,$1));}
	  | CONID                       {$$ = gc1(singleton($1));}
	  ;

/*- Processing definitions of primitives ----------------------------------*/

topDecl   : PRIMITIVE prims COCO sigType{primDefn($1,$2,$4); sp-=4;}
	  ;
prims     : prims ',' prim              {$$ = gc3(cons($3,$1));}
	  | prim                        {$$ = gc1(cons($1,NIL));}
	  | error                       {syntaxError("primitive defn");}
	  ;
prim      : var STRINGLIT               {$$ = gc2(pair($1,$2));}
	  | var                         {$$ = $1;}
	  ;

/*- Class declarations: ---------------------------------------------------*/

topDecl   : TCLASS crule wherePart      {classDefn(intOf($1),$2,$3); sp-=3;}
	  | TINSTANCE irule wherePart   {instDefn(intOf($1),$2,$3);  sp-=3;}
	  | DEFAULT '(' dtypes ')'      {defaultDefn(intOf($1),$3);  sp-=4;}
	  ;
crule     : context IMPLIES type        {$$ = gc3(pair($1,checkClass($3)));}
	  | type                        {$$ = gc1(pair(NIL,checkClass($1)));}
	  ;
irule     : context IMPLIES type        {$$ = gc3(pair($1,checkInst($3)));}
	  | type                        {$$ = gc1(pair(NIL,checkInst($1)));}
	  ;
dtypes    : /* empty */                 {$$ = gc0(NIL);}
	  | dtypes1                     {$$ = gc1(rev($1));}
	  ;
dtypes1   : dtypes1 ',' type            {$$ = gc3(cons($3,$1));}
	  | type                        {$$ = gc1(cons($1,NIL));}
	  ;

/*- Type expressions: -----------------------------------------------------*/
/*  Parser is not sufficently powerful to distinguish between a predicate
 *  such as "Dual a b" and a type "Sum a b", or between a tuple type and
 *  a context (e.g. (Alpha a, Beta b) is a tuple or context?).  For this
 *  reason, individual predicates and contexts are parsed as types, with
 *  additional code to check for well formed context/classes.
 */

sigType   : context IMPLIES type        {$$ = gc3(ap(QUAL,pair($1,$3)));}
	  | type                        {$$ = $1;}
	  ;
context   : type                        {$$ = gc1(checkContext($1));}
	  ;
type      : btype                       {$$ = $1;}
	  | btype ARROW type            {$$ = gc3(ap(ap(typeArrow,$1),$3));}
	  | error                       {syntaxError("type expression");}
	  ;
btype     : btype1                      {$$ = $1;}
	  | btype2                      {$$ = $1;}
	  ;
btype1    : btype1 atype                {$$ = gc2(ap($1,$2));}
	  | atype1                      {$$ = $1;}
	  ;
btype2    : btype2 atype                {$$ = gc2(ap($1,$2));}
	  | CONID                       {$$ = $1;}
	  ;
atype     : atype1                      {$$ = $1;}
	  | CONID                       {$$ = $1;}
	  ;
atype1    : varid1                      {$$ = $1;}
	  | '(' ')'                     {$$ = gc2(typeUnit);}
	  | '(' ARROW ')'               {$$ = gc3(typeArrow);}
	  | '(' type ')'                {$$ = gc3($2);}
	  | '(' tupCommas ')'           {$$ = gc3($2);}
	  | '(' typeTuple ')'           {$$ = gc3(buildTuple($2));}
	  | '[' type ']'                {$$ = gc3(ap(typeList,$2));}
	  | '[' ']'                     {$$ = gc2(typeList);}
	  | '_'                         {$$ = gc1(inventVar());}
	  ;
tupCommas : tupCommas ','               {$$ = gc2(mkTuple(tupleOf($1)+1));}
	  | ','                         {$$ = gc1(mkTuple(2));}
	  ;
typeTuple : typeTuple ',' type          {$$ = gc3(cons($3,$1));}
	  | type ',' type               {$$ = gc3(cons($3,cons($1,NIL)));}
	  ;

/*- Value declarations: ---------------------------------------------------*/

decllist  : '{' decls end               {$$ = gc3($2);}
	  ;
decls     : /* empty */                 {$$ = gc0(NIL);}
	  | ';'                         {$$ = gc1(NIL);}
	  | decls1                      {$$ = $1;}
	  | decls1 ';'                  {$$ = gc2($1);}
	  ;
decls1    : decls1 ';' decl             {$$ = gc3(cons($3,$1));}
	  | decl                        {$$ = gc1(cons($1,NIL));}
	  ;
decl      : vars COCO sigType           {$$ = gc3(sigdecl($2,$1,$3));}
	  | opExp rhs                   {$$ = gc2(pair($1,$2));}
	  ;
rhs       : rhs1 wherePart              {$$ = gc2(letrec($2,$1));}
	  | error                       {syntaxError("declaration");}
	  ;
rhs1      : '=' exp                     {$$ = gc2(pair($1,$2));}
	  | gdefs                       {$$ = gc1(grded(rev($1)));}
	  ;
wherePart : WHERE decllist              {$$ = gc2($2);}
	  | /*empty*/                   {$$ = gc0(NIL);}
	  ;
gdefs     : gdefs gdef                  {$$ = gc2(cons($2,$1));}
	  | gdef                        {$$ = gc1(cons($1,NIL));}
	  ;
gdef      : '|' exp '=' exp             {$$ = gc4(pair($3,pair($2,$4)));}
	  ;
vars      : vars ',' var                {$$ = gc3(cons($3,$1));}
	  | var                         {$$ = gc1(cons($1,NIL));}
	  ;
var       : varid                       {$$ = $1;}
	  | '(' '-' ')'                 {$$ = gc3(varMinus);}
	  ;
varid     : varid1                      {$$ = $1;}
	  | '(' VAROP ')'               {$$ = gc3($2);}
	  | '(' '!' ')'                 {$$ = gc3(varBang);}
	  ;
varid1    : VARID                       {$$ = $1;}
	  | HIDING                      {$$ = gc1(varHiding);}
	  | QUALIFIED                   {$$ = gc1(varQualified);}
	  | ASMOD                       {$$ = gc1(varAsMod);}
	  ;
conid     : CONID                       {$$ = $1;}
	  | '(' CONOP ')'               {$$ = gc3($2);}
	  ;

/*- Expressions: ----------------------------------------------------------*/

exp       : exp1                        {$$ = $1;}
	  | error                       {syntaxError("expression");}
	  ;
exp1      : opExp COCO sigType          {$$ = gc3(ap(ESIGN,pair($1,$3)));}
	  | opExp                       {$$ = $1;}
	  ;
opExp     : pfxExp                      {$$ = $1;}
	  | pfxExp op pfxExp            {$$ = gc3(ap(ap($2,$1),$3));}
	  | opExp0                      {$$ = gc1(tidyInfix($1));}
	  ;
opExp0    : opExp0 op pfxExp            {$$ = gc3(ap(ap($2,$1),$3));}
	  | pfxExp op pfxExp op pfxExp  {$$ = gc5(ap(ap($4,
							ap(ap($2,singleton($1)),
							   $3)),$5));}
	  ;
pfxExp    : '-' appExp                  {if (isInt($2))
					     $$ = gc2(mkInt(-intOf($2)));
					 else
					     $$ = gc2(ap(nameNegate,$2));
					}
	  | '\\' pats ARROW exp         {$$ = gc4(ap(LAMBDA,
						     pair(rev($2),
							  pair($3,$4))));}
	  | LET decllist IN exp         {$$ = gc4(letrec($2,$4));}
	  | IF exp THEN exp ELSE exp    {$$ = gc6(ap(COND,triple($2,$4,$6)));}
	  | CASEXP exp OF '{' alts end  {$$ = gc6(ap(CASE,pair($2,rev($5))));}
	  | DO '{' stmts end            {$$ = gc4(ap(DOCOMP,checkDo($3)));}
	  | appExp                      {$$ = $1;}
	  ;
pats      : pats atomic                 {$$ = gc2(cons($2,$1));}
	  | atomic                      {$$ = gc1(cons($1,NIL));}
	  ;
appExp    : appExp atomic               {$$ = gc2(ap($1,$2));}
	  | TRUNST atomic               {$$ = gc2(ap(RUNST,$2));}
	  | atomic                      {$$ = $1;}
	  ;
atomic    : var                         {$$ = $1;}
	  | var '@' atomic              {$$ = gc3(ap(ASPAT,pair($1,$3)));}
	  | '~' atomic                  {$$ = gc2(ap(LAZYPAT,$2));}
	  | '_'                         {$$ = gc1(WILDCARD);}
	  | conid                       {$$ = $1;}
	  | conid '{' fbinds '}'        {$$ = gc4(ap(CONFLDS,pair($1,$3)));}
	  | atomic '{' fbinds '}'       {$$ = gc4(ap(UPDFLDS,
						     triple($1,NIL,$3)));}
	  | '(' ')'                     {$$ = gc2(nameUnit);}
	  | NUMLIT                      {$$ = $1;}
	  | CHARLIT                     {$$ = $1;}
	  | STRINGLIT                   {$$ = $1;}
	  | REPEAT                      {$$ = $1;}
	  | '(' exp ')'                 {$$ = gc3($2);}
	  | '(' exps2 ')'               {$$ = gc3(buildTuple($2));}
	  | '[' list ']'                {$$ = gc3($2);}
	  | '(' pfxExp op ')'           {$$ = gc4(ap($3,$2));}
	  | '(' varop atomic ')'        {$$ = gc4(ap(ap(nameFlip,$2),$3));}
	  | '(' conop atomic ')'        {$$ = gc4(ap(ap(nameFlip,$2),$3));}
	  | '(' tupCommas ')'           {$$ = gc3($2);}
	  ;
exps2     : exps2 ',' exp               {$$ = gc3(cons($3,$1));}
	  | exp ',' exp                 {$$ = gc3(cons($3,cons($1,NIL)));}
	  ;
alts      : alts1                       {$$ = $1;}
	  | alts1 ';'                   {$$ = gc2($1);}
	  ;
alts1     : alts1 ';' alt               {$$ = gc3(cons($3,$1));}
	  | alt                         {$$ = gc1(cons($1,NIL));}
	  ;
alt       : opExp altRhs wherePart      {$$ = gc3(pair($1,letrec($3,$2)));}
	  ;
altRhs    : guardAlts                   {$$ = gc1(grded(rev($1)));}
	  | ARROW exp                   {$$ = gc2(pair($1,$2));}
	  | error                       {syntaxError("case expression");}
	  ;
guardAlts : guardAlts guardAlt          {$$ = gc2(cons($2,$1));}
	  | guardAlt                    {$$ = gc1(cons($1,NIL));}
	  ;
guardAlt  : '|' opExp ARROW exp         {$$ = gc4(pair($3,pair($2,$4)));}
	  ;
stmts     : stmts1 ';'                  {$$ = gc2($1);}
	  | stmts1                      {$$ = $1;}
	  ;
stmts1    : stmts1 ';' stmt             {$$ = gc3(cons($3,$1));}
	  | stmt                        {$$ = gc1(cons($1,NIL));}
	  ;
stmt      : exp1 FROM exp               {$$ = gc3(ap(FROMQUAL,pair($1,$3)));}
	  | LET decllist                {$$ = gc2(ap(QWHERE,$2));}
	  | IF exp                      {$$ = gc2(ap(BOOLQUAL,$2));}
	  | exp1                        {$$ = gc1(ap(DOQUAL,$1));}
	  ;
fbinds    : /* empty */                 {$$ = gc0(NIL);}
	  | fbinds1                     {$$ = gc1(rev($1));}
	  ;
fbinds1   : fbinds1 ',' fbind           {$$ = gc3(cons($3,$1));}
	  | fbind                       {$$ = gc1(singleton($1));}
	  ;
fbind     : var                         {$$ = $1;}
	  | var '=' exp                 {$$ = gc3(pair($1,$3));}
	  ;

/*- List Expressions: -------------------------------------------------------*/

list      : /* empty */                 {$$ = gc0(nameNil);}
	  | exp                         {$$ = gc1(ap(FINLIST,cons($1,NIL)));}
	  | exps2                       {$$ = gc1(ap(FINLIST,rev($1)));}
	  | exp '|' quals               {$$ = gc3(ap(COMP,pair($1,rev($3))));}
	  | exp         UPTO exp        {$$ = gc3(ap(ap(nameFromTo,$1),$3));}
	  | exp ',' exp UPTO            {$$ = gc4(ap(ap(nameFromThen,$1),$3));}
	  | exp         UPTO            {$$ = gc2(ap(nameFrom,$1));}
	  | exp ',' exp UPTO exp        {$$ = gc5(ap(ap(ap(nameFromThenTo,
							       $1),$3),$5));}
	  ;
quals     : quals ',' qual              {$$ = gc3(cons($3,$1));}
	  | qual                        {$$ = gc1(cons($1,NIL));}
	  ;
qual      : exp FROM exp                {$$ = gc3(ap(FROMQUAL,pair($1,$3)));}
	  | exp                         {$$ = gc1(ap(BOOLQUAL,$1));}
	  | LET decllist                {$$ = gc2(ap(QWHERE,$2));}
	  ;

/*- Tricks to force insertion of leading and closing braces ---------------*/

begin     : error                       {yyerrok; goOffside(startColumn);}
	  ;
					/* deal with trailing semicolon    */
end       : '}'                         {$$ = $1;}
	  | error                       {yyerrok;
					 if (canUnOffside()) {
					     unOffside();
					     /* insert extra token on stack*/
					     push(NIL);
					     pushed(0) = pushed(1);
					     pushed(1) = mkInt(column);
					 }
					 else
					     syntaxError("definition");
					}
	  ;

/*-------------------------------------------------------------------------*/

%%

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
