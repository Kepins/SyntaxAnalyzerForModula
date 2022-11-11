%{
#include	<stdio.h>
#include	<string.h>
#define MAX_STR_LEN	100

  int yylex(void);
  void yyerror(const char *txt);

  void found( const char *nonterminal, const char *value );
%}

%union {
  char s[ MAX_STR_LEN + 1 ];
  int i;
  double d;
}

%start GRAMMAR
/* keywords */
%token <i> KW_AND KW_BEGIN KW_CONST KW_DIV KW_DO KW_ELSE KW_ELSIF KW_END KW_FOR
%token <i> KW_FROM KW_IF KW_IMPORT KW_IN KW_MOD KW_MODULE KW_NOT KW_PROCEDURE
%token <i> KW_OR KW_THEN KW_TYPE KW_TO KW_VAR KW_WHILE KW_REPEAT KW_UNTIL
%token<i> KW_LOOP KW_CASE KW_OF KW_ARRAY KW_RECORD KW_DOWNTO
/* literal values */
%token <s> STRING_CONST CHAR_CONST
%token <i> INTEGER_CONST
%token <d> REAL_CONST
/* operators */
%token <I> ASSIGN LE GE NEQ RANGE
/* other */
%token <s> IDENT

%left '+' '-' KW_OR
%left '*' '/' KW_DIV KW_MOD KW_AND '&'
%left NEG KW_NOT
%type <s> PROGRAM_MODULE PROCEDURE_CALL FOR_STATEMENT ASSIGNMENT PROC_HEAD


%%

 /* GRAMMAR */
 /* Apart from what is given below, GRAMMAR can also be a program module
    (PROGRAM_MODULE) */
GRAMMAR: %empty { yyerror( "Empty input source is not valid!" ); YYERROR; }
        | PROGRAM_MODULE
	| error
	  /* start here */
;

/* PROGRAM_MODULE */
/* Consists of a keyword MODULE, followed by an identifier (IDENT),
   semicolon, import declaractions (IMPORTS), block (BLOCK),
   identifier and a dot (fullstop, period) */
PROGRAM_MODULE: KW_MODULE IDENT ';' IMPORTS BLOCK IDENT '.'  { found("PROGRAM_MODULE", $2); } ;

/* IMPORTS */
/* A possibly empty sequence of imports (IMPORT) */
IMPORTS:
	%empty {}
	| IMPORTS IMPORT
;


/* IMPORT */
/* Keyword FROM, followed by identifier, keyword IMPORT,
 list of identifiers, and a semicolon */
IMPORT: KW_FROM IDENT KW_IMPORT IDENT_LIST ';' { found("IMPORT", $2); };

/* IDENT_LIST */
/* List of identifiers separated with commas */
IDENT_LIST:
	IDENT
	| IDENT_LIST ',' IDENT { }
;


/* BLOCK */
/* Can either be a sequence:
   declarations (DECLARATIONS), keyword BEGIN, statements (STATEMENTS),
   and keyword END,
   or a sequence:
   declarations, and keyword END*/
BLOCK:
	DECLARATIONS KW_BEGIN STATEMENTS KW_END
	| DECLARATIONS KW_END
;
/* DECLARATIONS */
/* Possibly empty sequence of declarations (DECLARATION) */
DECLARATIONS:
	%empty {}
	| DECLARATIONS DECLARATION
;


/* DECLARATION */
/* Declaration is either keyword CONST, followed by declarations of constants
  (CONST_DECLS),
  or a keyword VAR, followed by declaration of variables (VAR_DECLS),
  or a procedure declaration (PROC_DECL) with a semicolon */
DECLARATION:
	KW_CONST CONST_DECLS
	| KW_VAR VAR_DECLS
	| PROC_DECL ';'
;


/* CONST_DECLS */
/* Possibly empty sequence of declarations of constants (CONST_DECL), each
   followed by a semicolon */
CONST_DECLS:
	%empty
	| CONST_DECLS CONST_DECL ';'
;

/* CONST_DECL */
/* Identifier, fololowed by equal sign and a constant factor (CONST_FACTOR) */
CONST_DECL: IDENT '=' CONST_FACTOR { found("CONST_DECL", $1); };

/* CONST_FACTOR */
/* Either identifier, integer constant, real constant or string */
CONST_FACTOR:
	IDENT
	| INTEGER_CONST
	| REAL_CONST
	| STRING_CONST
;

/* VAR_DECLS */
/* Possibly empty sequence of declarations of variables (VAR_DECL), each
   followed by a semicolon */
VAR_DECLS:
	%empty
	| VAR_DECLS VAR_DECL ';'
;

/* VAR_DECL */
/* List of identifiers (IDENT_LIST), followed by a colon, and type specification
   (TYPE_SPEC) */
VAR_DECL: IDENT_LIST ':' TYPE_SPEC { found("VAR_DECL", ""); };

/* TYPE_SPEC */
/* One of tyhe following:
   - identifier,
   - keyword ARRAY, specification of diomensions (DIMEN_SPECS), keyword OF,
     and type specification (TYPE_SPEC)
   - keyword RECORD, fields (FIELDS), keyword END
*/
TYPE_SPEC:
	IDENT
	| KW_ARRAY DIMEN_SPECS KW_OF TYPE_SPEC
	| KW_RECORD FIELDS KW_END
;
/* DIMEN_SPECS */
 /* nonempty list of dimension specifications (DIMEN_SPEC)
    separated with commas */
DIMEN_SPECS:
	DIMEN_SPEC
	| DIMEN_SPECS ',' DIMEN_SPEC
;
/* DIMEN_SPEC */
/* left square bracket, constant factor (CONST_FACTOR), range operator,
   constant factor, right square  bracket */
DIMEN_SPEC: '[' CONST_FACTOR RANGE CONST_FACTOR ']' ;
/* FIELDS */
/* Possibly empty sequence of fields (FIELD), followed each by a semicolon */
FIELDS:
	%empty
	| FIELDS FIELD ';'
;

/* FIELD */
/* Identifier list (IDENT_LIST), followed by a colon, and type specification
   (TYPE_SPEC) */
FIELD: IDENT_LIST ':' TYPE_SPEC ;

/* PROC_DECL */
/* Procedure header, semicolon, block, and identifier */
PROC_DECL: PROC_HEAD ';' BLOCK IDENT { found("PROCEDURE_DECL", $4); };

/* PROC_HEAD */
/* keyword PROCEDURE (KW_PROCEDURE), identifier (IDENT),
   optional formal parameters (OPT_FORMAL_PARAMS) */
PROC_HEAD: KW_PROCEDURE IDENT OPT_FORMAL_PARAMS { found("PROC_HEAD", $2); };

/* OPT_FORMAL_PARAMS */
/* empty or formal parameters (FORMAL_PARAMS) */
OPT_FORMAL_PARAMS:
	%empty
	| FORMAL_PARAMS
;
/* FORMAL_PARAMS */
/* left parenthesis, formal parameter sections (FP_SECTIONS),
   right parenthesis, optional return type (OPT_RET_TYPE) */
FORMAL_PARAMS: '(' FP_SECTIONS ')' OPT_RET_TYPE ;

	
/* FP_SECTIONS */
/* nonempty list of formal parameter sections (FP_SECTION) separated with
   semicolons */
FP_SECTIONS:
	FP_SECTION
	| FP_SECTIONS ';' FP_SECTION
;

/* FP_SECTION */
/* optional keyword VAR (OPT_VAR), identifier list (IDENT_LIST),
   colon, and qualified indentifier (QUALIDENT) */
FP_SECTION: OPT_VAR IDENT_LIST ':' QUALIDENT { found("FP_SECTION", ""); };


/* OPT_VAR */
/* empty or keyword VAR (KW_VAR) */
OPT_VAR:
	%empty
	| KW_VAR
;

/* OPT_RET_TYPE */
/* empty or colon and qualified identifier (QUALIDENT) */
OPT_RET_TYPE:
	%empty
	| ':' QUALIDENT
;


/* QUALIDENT */
/* nonempty identifier list separated with dots */
QUALIDENT:
	IDENT
	| QUALIDENT '.' IDENT
;
/* STATEMENTS */
/* List of statements (STATEMENT) separated with semicolons */
STATEMENTS:
	STATEMENT
	| STATEMENTS ';'  STATEMENT
;

	
/* STATEMENT */
/* Either a procedure call (PROCEDURE_CALL),
 or assignment (ASSIGNMENT),
 or a for loop(FOR_STATEMENT),
 or conditional instruction (IF_STATEMENT),
 or while loop (WHILE_STATEMENT),
 or repeat loop (REPEAT_STATEMENT),
 or infinite loop (LOOP_STATEMENT),
 or case statement (CASE_STATEMENT)
*/
STATEMENT:
	PROCEDURE_CALL
	| ASSIGNMENT
	| FOR_STATEMENT
	| IF_STATEMENT
	| WHILE_STATEMENT
	| REPEAT_STATEMENT
	| LOOP_STATEMENT
	| CASE_STATEMENT
	
;


/* ASSIGNMENT */
/* Identifier, qualifier assignment operator (ASSIGN), and expression (EXPR) */
ASSIGNMENT: IDENT QUALIF ASSIGN EXPR { found("ASSIGNMENT", $1);  };


/* QUALIF */
/* can either be empty,
   or a left square bracket, indexes (SUBSCRIPTS), right square  bracket,
      and qualifier,
   or a dot, identifier, and qualifier */
QUALIF:
	%empty
	| '[' SUBSCRIPTS ']' QUALIF
	| '.' IDENT QUALIF
;

/* SUBSCRIPTS */
/* nonepmty expression list separated with commas */
SUBSCRIPTS:
	EXPR
	| SUBSCRIPTS ',' EXPR
;


/* PROCEDURE_CALL */
/* Either only identifier,
 or identifier, left parenthesis, actual parameters (ACT_PARAMETERS),
 and right parenthesis */
PROCEDURE_CALL:
	IDENT { found("PROCEDURE_CALL", $1); }
	| IDENT '(' ACT_PARAMETERS ')' { found("PROCEDURE_CALL", $1); }
;


/* ACT_PARAMETERS */
/* List of expressions (EXPR) separated with commas */
ACT_PARAMETERS:
	EXPR
	| ACT_PARAMETERS ',' EXPR
;
/* EXPR */
/* Either a simple expression (SIMPLE_EXPR),
   or two simple expressions separated with a relational operator (REL_OP) */
EXPR:
	SIMPLE_EXPR
	| SIMPLE_EXPR REL_OP SIMPLE_EXPR
;
	
/* REL_OP */
/* Either equal sign, less, greater, hash, not equal (NEQ),
   less or equal (LE), greater or equal (GE), or keyword IN */
REL_OP:
	'='
	| '<'
	| '>'
	| '#'
	| NEQ
	| LE
	| GE
	| KW_IN
;

/* SIMPLE_EXPR */
/* Either a factor (FACTOR),
   or a pair of simple expressions separated with an operator of:
   - addition,
   - subtraction,
   - logical sum (KW_OR),
   - multiplication,
   - division,
   - conjunction (KW_AND),
   - interger division (KW_DIV),
   - or modulo (KW_MOD) */
SIMPLE_EXPR:
	FACTOR
	| SIMPLE_EXPR '+' SIMPLE_EXPR
	| SIMPLE_EXPR '-' SIMPLE_EXPR
	| SIMPLE_EXPR KW_OR SIMPLE_EXPR
	| SIMPLE_EXPR '*' SIMPLE_EXPR
	| SIMPLE_EXPR '/' SIMPLE_EXPR
	| SIMPLE_EXPR KW_AND SIMPLE_EXPR
	| SIMPLE_EXPR KW_DIV SIMPLE_EXPR
	| SIMPLE_EXPR KW_MOD SIMPLE_EXPR
;

/* FACTOR */
/* Either interger constant, real constant, string constant,
   or character constant,
   identifier with a qualifier,
   procedure call, expression in parentheses,
   or a factor following either keyword NOT, or a unary minus
   (with NEG precedence) */
FACTOR:
	INTEGER_CONST
	| REAL_CONST
	| STRING_CONST
	| CHAR_CONST
	| IDENT QUALIF
	| PROCEDURE_CALL
	| '(' EXPR ')'
	| KW_NOT FACTOR
	| NEG FACTOR
;

/* FOR_STATEMENT */
 /* Sequence: keyword FOR, identifier, assignment operator (ASSIGN),
    expression, TO or DOWNTO (TO_DOWNTO), expression, keyword DO,
   statements (STATEMENTS), and keyword END */
FOR_STATEMENT: KW_FOR IDENT ASSIGN EXPR TO_DOWNTO EXPR KW_DO STATEMENTS KW_END { found("FOR_STATEMENT", $2); } ;

/* TO_DOWNTO */
 /* Either keyword TO, or keyword DOWNTO */
TO_DOWNTO:
	KW_TO
	| KW_DOWNTO
;

/* IF_STATEMENT */
/* Sequence: keyword IF, expression,
   keyword THEN, statements (STATEMENTS),  ELSIF part (ELSIFS),
   ELSE part (ELSE_PART), and keyword END */
IF_STATEMENT: KW_IF EXPR KW_THEN STATEMENTS ELSIFS ELSE_PART KW_END { found("IF_STATEMENT", ""); } ;

/* ELSIFS */
/* Possibly empty sequence of sequences of the form:
   keyword ELSIF, expression, keyword THEN, and statements */
ELSIFS:
	%empty
	| ELSIFS KW_ELSIF EXPR KW_THEN STATEMENTS
;

/* ELSE_PART */
/* Either empty or keyword ELSE followed by statements */
ELSE_PART:
	%empty
	| KW_ELSE STATEMENTS
;


/* WHILE_STATEMENT */
/* keyword WHILE (KW_WHILE), expression (EXPR), keyword DO (KW_DO),
   statements (STATEMENTS), and keyword END (KW_END) */
WHILE_STATEMENT: KW_WHILE EXPR KW_DO STATEMENTS KW_END { found("WHILE_STATEMENT", ""); } ;


/* REPEAT_STATEMENT */
/* keyword REPEAT (KW_REPEAT), statements (STATEMENTS),
   keyword UNTIL (KW_UNTIL), and expression (EXPR) */
REPEAT_STATEMENT: KW_REPEAT STATEMENTS KW_UNTIL EXPR {found("REPEAT_STATEMENT", ""); } ;


/* LOOP_STATEMENT */
/* keyword LOOP (KW_LOOP), statements (STATEMENTS), and keyword end (KW_END) */
LOOP_STATEMENT: KW_LOOP STATEMENTS KW_END {found("LOOP_STATEMENT", ""); } ;

/* CASE_STATEMENT */
/* keyword CASE (KW_CASE), expression (EXPR), keyword OF,
   cases (CASES), ELSE part (ELSE_PART), and keyword END */
CASE_STATEMENT: KW_CASE EXPR KW_OF CASES ELSE_PART KW_END { found("CASE_STATEMENT", ""); };

/* CASES */
/* lists of 3 items: CASE labels (CASE_LABELS), colon,
   and statements (STATEMENTS), every 3 items separated with vertical bars */
CASES:
	CASE_LABELS ':' STATEMENTS
	| CASES '|' CASE_LABELS ':' STATEMENTS
;

/* CASE_LABELS */
/* list of CASE_LABEL separated with commas */
CASE_LABELS:
	CASE_LABEL
	| CASE_LABELS ',' CASE_LABEL
;


/* CASE_LABEL */
/* Either an integer constant, or a character constant */
CASE_LABEL:
	INTEGER_CONST
	| CHAR_CONST
;

%%

int main( void )
{ 
	printf( "Szymon Kepinski\n" );
	printf( "yytext              Token type      Token value as string\n\n" );
	yyparse();
	return( 0 ); // OK
}

void yyerror( const char *txt)
{
	printf( "%s\n", txt );
}

void found( const char *nonterminal, const char *value )
{  /* info on found syntactic structures (nonterminal) */
        printf( "===== FOUND: %s %s%s%s=====\n", nonterminal, 
                        (*value) ? "'" : "", value, (*value) ? "'" : "" );
}
