/* File: parser.y
 * --------------
 * Bison input file to generate the parser for the compiler.
 *
 * pp2: your job is to write a parser that will construct the parse tree
 *      and if no parse errors were found, print it.  The parser should
 *      accept the language as described in specification, and as augmented
 *      in the pp2 handout.
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(const char *msg); // standard error-handling routine

%}

/* The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */

/* yylval
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser.
 *
 * pp2: You will need to add new fields to this union as you add different
 *      attributes to your non-terminal symbols.
 */
%union {
    int integerConstant;
    bool boolConstant;
    float floatConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    
    Identifier *ide;
    Decl *decl;
    List<Decl*> *declList;
    List<VarDecl*> *vdeclList;
    VarDecl *vDecl;
    FnDecl *fDecl;
    TypeQualifier *typeq;
    Type *type;
    ArrayType *arrType;
    NamedType *nameType;
    Expr *expr;
    EmptyExpr *emptyExpr;
    CompoundExpr *compExpr;
    ArithmeticExpr *arithExpr;
    RelationalExpr *relExpr;
    EqualityExpr *eqExpr;
    LogicalExpr *logExpr;
    SelectionExpr *selExpr;
    AssignExpr *aExpr;
    PostfixExpr *postExpr;
    VarExpr *vExpr;
    LValue *lval;
    FieldAccess *fAccess;
    ArrayAccess *arrAccess;
    Operator *op;
    Stmt *stmt;
    List <Stmt*> *stmtList;
    List <Case*> *caList;
    StmtBlock *sBlock;
    ConditionalStmt *condStmt;
    LoopStmt *loop;
    ForStmt *forSt;
    WhileStmt *whileSt;
    DoWhileStmt *dwhileSt;
    IfStmt *ifSt;
    BreakStmt *brkStmt;
    ReturnStmt *retSt;
    SwitchLabel *sLabel;
    Case *ca;
    Default *def;
    SwitchStmt *switchSt;
    
}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Bison will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */
%token   T_Void T_Bool T_Int T_Float
%token   T_LessEqual T_GreaterEqual T_EQ T_NE T_LeftAngle T_RightAngle
%token   T_And T_Or
%token   T_Equal T_MulAssign T_DivAssign T_AddAssign T_SubAssign
%token   T_While T_For T_If T_Else T_Return T_Break
%token   T_Const T_Uniform T_Layout T_Continue T_Do
%token   T_Inc T_Dec T_Switch T_Case T_Default
%token   T_In T_Out T_InOut
%token   T_Mat2 T_Mat3 T_Mat4 T_Vec2 T_Vec3 T_Vec4
%token   T_Ivec2 T_Ivec3 T_Ivec4 T_Bvec2 T_Bvec3 T_Bvec4
%token   T_Uint T_Uvec2 T_Uvec3 T_Uvec4 T_Struct
%token   T_Semicolon T_Dot T_Colon T_Question T_Comma
%token   T_Dash T_Plus T_Star T_Slash
%token   T_LeftParen T_RightParen T_LeftBracket T_RightBracket T_LeftBrace T_RightBrace

%token   <identifier> T_Identifier
%token   <integerConstant> T_IntConstant
%token   <floatConstant> T_FloatConstant
%token   <boolConstant> T_BoolConstant

/* Non-terminal types
 * ------------------
 * In order for yacc to assign/access the correct field of $$, $1, we
 * must to declare which field is appropriate for the non-terminal.
 * As an example, this first type declaration establishes that the DeclList
 * non-terminal uses the field named "declList" in the yylval union. This
 * means that when we are setting $$ for a reduction for DeclList ore reading
 * $n which corresponds to a DeclList nonterminal we are accessing the field
 * of the union named "declList" which is of type List<Decl*>.
 * pp2: You'll need to add many of these of your own.
 */
%type <declList>	DeclList

%type <decl>		ExDecl Decl

%type <vDecl> 		SinDecl
%type <vdeclList>	ParamList

%type <fDecl> 		FnDecl FnDef
%type <fDecl>		FnProto FnHeader FnHeaderParam

%type <type>		FullType TypeSpecifier
%type <typeq>		TypeQual SinTypeQual StorageQual
%type <type>		TSpecNonArr

%type <ide>		VarIdentifier FnCallHeader

%type <expr>		PrimaryExpr PostFixExpr IntExpr
%type <expr>		FnCall FnCallOrMethod FnCallGen
%type <expr>		FnCallWithParam FnCallWithNoParam
%type <expr>		UnaryExpr MultExpr AddExpr
%type <expr>		ShiftExpr RelExpr EqExpr
%type <expr>		AndExpr XorExpr OrExpr
%type <expr>		LogAndExpr LogXorExpr LogOrExpr
%type <expr>		CondExpr AssignEx
%type <expr>		Expr ConstExpr
%type <expr>		Initial Condition CondOpt
%type <expr>		ExStmt

%type <stmt>		Statement SimpleStmt
%type <stmt>		StmtWScp CompStmtWScp
%type <stmt>		SelStmt SwtStmt
%type <stmt>		ItStmt JmpStmt

%type <stmtList>	StmtList 

%type <ca>		CaseStmt
%type <caList>		CaseStmtlist SwtStmtList

%type <def>		Default


%type <op>		UnaryOp AssignOp

%nonassoc "then"
%nonassoc T_Else
%nonassoc "NoDef"
%nonassoc "Def"

%left T_Plus T_Dash
%left T_Star T_Slash

%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.

 */
Program   :    DeclList            {
                                      @1;
                                      /* pp2: The @1 is needed to convince
                                       * yacc to set up yylloc. You can remove
                                       * it once you have other uses of @n*/
                                      Program *program = new Program($1);
                                      // if no errors, advance to next phase
                                      if (ReportError::NumErrors() == 0)
                                          program->Print(0);
                                    }
          ;

DeclList	:	DeclList ExDecl		{ ($$=$1)->Append($2); }
          	|	ExDecl			{ ($$ = new List<Decl*>)->Append($1); }
          	;

ExDecl	  	:	FnDef			{ $$ = $1; }
		|	Decl			{ $$ = $1; }
		;

FnDef		:	FnProto	CompStmtWScp	{ $1 -> SetFunctionBody($2); 
						  $$ = $1;	
						}
		;

Decl		:	FnProto T_Semicolon			{ $$ = $1; }
		|	SinDecl T_Semicolon			{ $$ = $1; }
		;



FnProto		:	FnDecl T_RightParen			{ $$ = $1; }
		;

FnDecl		:	FnHeader				{ $$ = $1; }
		|	FnHeaderParam				{ $$ = $1; }
		;

FnHeaderParam	:	FullType T_Identifier T_LeftParen ParamList			{ $$ = new FnDecl(new Identifier(@2,$2),$1,$4); }
		|	TypeQual FullType T_Identifier T_LeftParen ParamList		{ $$ = new FnDecl(new Identifier(@3,$3),$2,$1,$5); }
		;

ParamList	:	ParamList T_Comma SinDecl				{ ($$ = $1) -> Append($3); }
		|	ParamList T_Semicolon SinDecl 				{ ($$ = $1) -> Append($3); }
		|	SinDecl							{ ($$ = new List<VarDecl*>()) -> Append($1); }
		;

FnHeader	:	FullType T_Identifier T_LeftParen		{  
							   	  	  Identifier *id = new Identifier(@2,$2);
							   	  	  $$ =  new FnDecl(id, $1, new List<VarDecl*> ());
									}
		|	TypeQual FullType T_Identifier T_LeftParen	{ $$ = new FnDecl(new Identifier(@3,$3),$2,$1,new List<VarDecl*>()); }
		;
 




VarIdentifier	:	T_Identifier		{ $$ = new Identifier (@1,$1);}
		;

PrimaryExpr	:	VarIdentifier			{ $$ = new VarExpr(@1,$1); }
		|	T_IntConstant			{ $$ = new IntConstant(@1,$1); }
		|	T_FloatConstant			{ $$ = new FloatConstant(@1,$1); }
		|	T_BoolConstant			{ $$ = new BoolConstant(@1,$1); }
		|	T_LeftParen Expr T_RightParen	{ $$ = $2; }
		;

PostFixExpr	:	PrimaryExpr						{ $$ = $1; }
		|	PostFixExpr T_LeftBracket IntExpr T_RightBracket	{ $$ = new ArrayAccess(@1,$1,$3); }
		|	FnCall							{ $$ = $1; }
		|	PostFixExpr T_Dot T_Identifier				{ Identifier *id = new Identifier(@3,$3);
								  		  $$ = new FieldAccess($1,id);
										}
		|	PostFixExpr T_Inc					{ Operator *op = new Operator(@2,"++");
								  		  $$ = new PostfixExpr($1,op);
										}
		|	PostFixExpr T_Dec					{ $$ = new PostfixExpr($1, new Operator(@2,"--")); }
		;

IntExpr		:	Expr			{ $$ = $1; }
		;

UnaryExpr	:	PostFixExpr		{ $$ = $1; }
		|	T_Inc UnaryExpr		{ $$ = new ArithmeticExpr(new Operator(@1,"++"),$2); }
		|	T_Dec UnaryExpr		{ $$ = new ArithmeticExpr(new Operator(@1,"--"),$2); }
		|	UnaryOp UnaryExpr	{ $$ = new ArithmeticExpr($1,$2); }
		;

UnaryOp		:	T_Plus			{ $$ = new Operator(@1,"+"); }
		|	T_Dash			{ $$ = new Operator(@1,"-"); }
		;

MultExpr	:	UnaryExpr 			{ $$ = $1; }
		|	MultExpr T_Star UnaryExpr	{ $$ = new ArithmeticExpr($1,new Operator(@2,"*"),$3); }
		|	MultExpr T_Slash UnaryExpr 	{ $$ = new ArithmeticExpr($1,new Operator(@2,"/"),$3); }
		;

AddExpr		:	MultExpr			{ $$ = $1; }
		|	AddExpr T_Plus MultExpr		{ $$ = new ArithmeticExpr($1,new Operator(@2,"+"),$3); }
		|	AddExpr T_Dash MultExpr		{ $$ = new ArithmeticExpr($1,new Operator(@2,"-"),$3); }
		;

ShiftExpr	:	AddExpr				{ $$ = $1; }
		;

RelExpr		:	ShiftExpr				 { $$ = $1; }
		|	RelExpr T_LeftAngle ShiftExpr		 { $$ = new RelationalExpr($1,new Operator(@2,"<"),$3); }
		|	RelExpr T_RightAngle ShiftExpr		 { $$ = new RelationalExpr($1,new Operator(@2,">"),$3); }
		|	RelExpr T_LessEqual ShiftExpr		 { $$ = new RelationalExpr($1,new Operator(@2,"<="),$3); }
		|	RelExpr T_GreaterEqual ShiftExpr  	{ $$ = new RelationalExpr($1,new Operator(@2,">="),$3); }
		;

EqExpr		:	RelExpr			{ $$ = $1; }
		|	EqExpr T_EQ RelExpr	{ $$ = new EqualityExpr($1, new Operator(@2,"=="),$3); }
		|	EqExpr T_NE RelExpr	{ $$ = new EqualityExpr($1, new Operator(@2,"!="),$3); }
		;

AndExpr		:	EqExpr		{ $$ = $1; }
		;

XorExpr		:	AndExpr		{ $$ = $1; }
		;

OrExpr		:	XorExpr		{ $$ = $1; }
		;
	
LogAndExpr	:	OrExpr			{ $$ = $1; }
		|	LogAndExpr T_And OrExpr	{ $$ = new LogicalExpr($1,new Operator(@2,"&&"),$3); }
		;

LogXorExpr	:	LogAndExpr	{ $$ = $1; }
		;

LogOrExpr	:	LogXorExpr			{ $$ = $1; }
		|	LogOrExpr T_Or LogXorExpr	{ $$ = new LogicalExpr($1,new Operator(@2,"||"),$3); }
		;

CondExpr	:	LogOrExpr					{ $$ = $1; }
		|	LogOrExpr T_Question Expr T_Colon AssignEx	{ $$ = new SelectionExpr($1,$3,$5); }
		;

AssignEx	:	CondExpr			{ $$ = $1; }
		|	UnaryExpr AssignOp AssignEx	{ $$ = new AssignExpr($1,$2,$3); }
		|	T_Vec2 T_LeftParen Expr T_RightParen	{ List<Expr*> *exList = new List<Expr*>();
						  		  exList -> Append($3);
								  $$ = new Call(@1,NULL,new Identifier(@1,"vec2"),exList);
								}
		|	T_Vec3 T_LeftParen Expr T_RightParen	{ List<Expr*> *exList = new List<Expr*>();
						  		  exList -> Append($3);
								  $$ = new Call(@1,NULL,new Identifier(@1,"vec3"),exList);
								}
		|	T_Vec4 T_LeftParen Expr T_RightParen	{ List<Expr*> *exList = new List<Expr*>();
						  	  	  exList -> Append($3);
						  		  $$ = new Call(@1,NULL,new Identifier(@1,"vec4"),exList);
								}
		;

AssignOp	:	T_Equal		{ $$ = new Operator(@1,"="); }
		|	T_MulAssign	{ $$ = new Operator(@1,"*="); }
		|	T_DivAssign	{ $$ = new Operator(@1,"/="); }
		|	T_AddAssign	{ $$ = new Operator(@1,"+="); }
		|	T_SubAssign	{ $$ = new Operator(@1,"-="); }
		;

Expr		:	AssignEx	{ $$ = $1; }
		;

ConstExpr	:	CondExpr	{ $$ = $1; }
		;


FnCall		:	FnCallOrMethod		{ $$ = $1; }
		;

FnCallOrMethod	:	FnCallGen		{ $$ = $1; }
		;

FnCallGen	: 	FnCallWithParam T_RightParen	{ $$ = $1; }
		|	FnCallWithNoParam T_RightParen	{ $$ = $1; }	
		;

FnCallWithNoParam:	FnCallHeader T_Void	{ $$ = new Call (@1,NULL,$1,new List<Expr*>()); }
		|	FnCallHeader		{ $$ = new Call (@1,NULL,$1,new List<Expr*>()); }
		;

FnCallWithParam	:	FnCallHeader AssignEx 			{ List<Expr*> *exList = new List<Expr*>(); 
								  exList -> Append($2);
								  $$ = new Call(@1,NULL,$1,exList);
								}
		| 	FnCallHeader AssignEx T_Comma AssignEx	{ List<Expr*> *exList = new List<Expr*>();
								  exList -> Append($2);
								  exList -> Append($4);
								  $$ = new Call(@1,NULL,$1,exList);
								}
		;


FnCallHeader	:	T_Identifier T_LeftParen		{ $$ = new Identifier(@1,$1); }
		;




SinDecl		:	FullType T_Identifier							{ $$ = new VarDecl(new Identifier(@2,$2),$1); }
		|	TypeQual FullType T_Identifier						{ $$ = new VarDecl(new Identifier(@3,$3),$2,$1); }
		|	FullType T_Identifier T_LeftBracket ConstExpr T_RightBracket		{ $$ = new VarDecl(new Identifier(@2,$2),new ArrayType(@1,$1)); }
		|	TypeQual FullType T_Identifier T_LeftBracket ConstExpr T_RightBracket	{ $$ = new VarDecl(new Identifier(@3,$3),new ArrayType(@2,$2),$1); }
		|	FullType T_Identifier T_Equal Initial  					{ $$ = new VarDecl(new Identifier(@2,$2),$1,$4); }
		|	TypeQual FullType T_Identifier T_Equal Initial				{ $$ = new VarDecl(new Identifier(@3,$3),$2,$1,$5); }
		;

FullType	:	TypeSpecifier		{ $$ = $1; }
		;

TypeQual	:	SinTypeQual		{ $$ = $1; }
		;
	
SinTypeQual	:	StorageQual		{ $$ = $1; }
		;

StorageQual	:	T_Const		{ $$ = TypeQualifier::constTypeQualifier; }
		|	T_In		{ $$ = TypeQualifier::inTypeQualifier; }
		| 	T_Out	  	{ $$ = TypeQualifier::outTypeQualifier; }
		|	T_Uniform	{ $$ = TypeQualifier::uniformTypeQualifier; }
		;

TypeSpecifier	:	TSpecNonArr		{ $$ = $1; }
		;


TSpecNonArr	:	T_Void		{ $$ = Type::voidType; }
		|	T_Float		{ $$ = Type::floatType; }
		|	T_Int		{ $$ = Type::intType; }
		|	T_Uint		{ $$ = Type::uintType; }
		|	T_Bool		{ $$ = Type::boolType; }
		|	T_Vec2		{ $$ = Type::vec2Type; }
		|	T_Vec3		{ $$ = Type::vec3Type; }
		|	T_Vec4		{ $$ = Type::vec4Type; }
		|	T_Bvec2		{ $$ = Type::bvec2Type; }
		| 	T_Bvec3		{ $$ = Type::bvec3Type; }
		|	T_Bvec4		{ $$ = Type::bvec4Type; }
		|	T_Ivec2		{ $$ = Type::ivec2Type; }
		|	T_Ivec3		{ $$ = Type::ivec3Type; }
		| 	T_Ivec4		{ $$ = Type::ivec4Type;	}
		|	T_Uvec2		{ $$ = Type::uvec2Type;	}
		|	T_Uvec3		{ $$ = Type::uvec3Type; }
		|	T_Uvec4		{ $$ = Type::uvec4Type; }
		|	T_Mat2		{ $$ = Type::mat2Type; }
		|	T_Mat3		{ $$ = Type::mat3Type; }
		|	T_Mat4		{ $$ = Type::mat4Type; }
		;

Initial		:	AssignEx		{ $$ = $1; }
		;

Statement	:	CompStmtWScp		{ $$ = $1; }
		|	SimpleStmt		{ $$ = $1; }
		;

StmtWScp	:	CompStmtWScp		{ $$ = $1; }
		|	SimpleStmt		{ $$ = $1; }
		;

SimpleStmt	:	ExStmt			{ $$ = $1; }
		|	SelStmt			{ $$ = $1; }
		|	SwtStmt			{ $$ = $1; }
		|	ItStmt			{ $$ = $1; }
		|	JmpStmt			{ $$ = $1; }
		;

CompStmtWScp	:	T_LeftBrace T_RightBrace			{ $$ = new StmtBlock(new List<VarDecl*>(), new List<Stmt*>()); }
		|	T_LeftBrace StmtList T_RightBrace		{ $$ = new StmtBlock(new List<VarDecl*>(),$2); }
		|	T_LeftBrace ParamList T_Semicolon StmtList T_RightBrace 	{ $$ = new StmtBlock($2,$4); }
		|	T_LeftBrace ParamList T_Semicolon T_RightBrace		{ $$ = new StmtBlock($2,new List<Stmt*>()); }
		;

StmtList	:	Statement		{ ($$ = new List<Stmt*>()) -> Append($1); }
		|	StmtList Statement	{ ($$ = $1) -> Append($2); }
		;

ExStmt		:	Expr T_Semicolon		{ $$ = $1; }
		;

SelStmt		:	T_If T_LeftParen Expr T_RightParen StmtWScp	%prec "then"		{ $$ = new IfStmt($3,$5,NULL); }
		|	T_If T_LeftParen Expr T_RightParen StmtWScp T_Else StmtWScp		{ $$ = new IfStmt($3,$5,$7); }
		;

Condition	:	Expr			{ $$ = $1;}
		;

SwtStmt		:	T_Switch T_LeftParen Expr T_RightParen T_LeftBrace SwtStmtList Default T_RightBrace %prec "Def"		{ $$ = new SwitchStmt($3,$6,$7); } 
                |	T_Switch T_LeftParen Expr T_RightParen T_LeftBrace SwtStmtList T_RightBrace %prec "Nodef"		{ $$ = new SwitchStmt($3,$6,NULL); } 
		;

SwtStmtList	:	/* nothing */		{  }
		|	CaseStmtlist		{ $$ = $1; }
		;

CaseStmtlist    :	CaseStmtlist CaseStmt	{ ($$ = $1) -> Append($2); }
		|	CaseStmt 		{ ($$ = new List<Case*>()) -> Append($1); }
		;

CaseStmt	:	T_Case Expr T_Colon		{ $$ = new Case($2,new List<Stmt*>()); }
		|	T_Case Expr T_Colon StmtList 	{ $$ = new Case($2,$4); }
		;


Default		:	T_Default T_Colon StmtList		{ $$ = new Default($3);}
		|	T_Default T_Colon			{ $$ = new Default(new List<Stmt*>()); }
		;


ItStmt		:	T_While T_LeftParen Condition T_RightParen StmtWScp					{ $$ = new WhileStmt($3,$5); }
		|	T_Do StmtWScp T_While T_LeftParen Expr T_RightParen T_Semicolon				{ $$ = new DoWhileStmt($2,$5); }
		|	T_For T_LeftParen Expr T_Semicolon CondOpt T_Semicolon Expr T_RightParen StmtWScp	{ $$ = new ForStmt($3,$5,$7,$9); }
		|	T_For T_LeftParen T_Semicolon CondOpt T_Semicolon Expr T_RightParen StmtWScp		{ $$ = new ForStmt(new EmptyExpr(),$4,$6,$8); }
		|	T_For T_LeftParen T_Semicolon T_Semicolon Expr T_RightParen StmtWScp			{ $$ = new ForStmt(new EmptyExpr(),new EmptyExpr(),$5,$7); }
		;

		;

CondOpt		:	Condition		{ $$ = $1; }
		;

JmpStmt		:	T_Break T_Semicolon		{ $$ = new BreakStmt(@1); }
		|	T_Return T_Semicolon		{ $$ = new ReturnStmt(@1,new EmptyExpr()); }
		|	T_Return Expr T_Semicolon	{ $$ = new ReturnStmt(@1,$2); }
		;



%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

/* Function: InitParser
 * --------------------
 * This function will be called before any calls to yyparse().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the parser (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yydebug that controls whether yacc prints debugging information about
 * parser actions (shift/reduce) and contents of state stack during parser.
 * If set to false, no information is printed. Setting it to true will give
 * you a running trail that might be helpful when debugging your parser.
 * Please be sure the variable is set to false when submitting your final
 * version.
 */
void InitParser()
{
   PrintDebug("parser", "Initializing parser");
   yydebug = false;
}
