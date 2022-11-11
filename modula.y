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
GRAMMAR: { yyerror( "Empty input source is not valid!" ); YYERROR; }
	| error
	  /* start here */
;

/* PROGRAM_MODULE */
/* Consists of a keyword MODULE, followed by an identifier (IDENT),
   semicolon, import declaractions (IMPORTS), block (BLOCK),
   identifier and a dot (fullstop, period) */

/* IMPORTS */
/* A possibly empty sequence of imports (IMPORT) */

/* IMPORT */
/* Keyword FROM, followed by identifier, keyword IMPORT,
 list of identifiers, and a semicolon */

/* IDENT_LIST */
/* List of identifiers separated with commas */

/* BLOCK */
/* Can either be a sequence:
   declarations (DECLARATIONS), keyword BEGIN, statements (STATEMENTS),
   and keyword END,
   or a sequence:
   declarations, and keyword END*/

/* DECLARATIONS */
/* Possibly empty sequence of declarations (DECLARATION) */

/* DECLARATION */
/* Declaration is either keyword CONST, followed by declarations of constants
  (CONST_DECLS),
  or a keyword VAR, followed by declaration of variables (VAR_DECLS),
  or a procedure declaration (PROC_DECL) with a semicolon */

/* CONST_DECLS */
/* Possibly empty sequence of declarations of constants (CONST_DECL), each
   followed by a semicolon */

/* CONST_DECL */
/* Identifier, fololowed by equal sign and a constant factor (CONST_FACTOR) */

/* CONST_FACTOR */
/* Either identifier, integer constant, real constant or string */

/* VAR_DECLS */
/* Possibly empty sequence of declarations of variables (VAR_DECL), each
   followed by a semicolon */

/* VAR_DECL */
/* List of identifiers (IDENT_LIST), followed by a colon, and type specification
   (TYPE_SPEC) */

/* TYPE_SPEC */
/* One of tyhe following:
   - identifier,
   - keyword ARRAY, specification of diomensions (DIMEN_SPECS), keyword OF,
     and type specification (TYPE_SPEC)
   - keyword RECORD, fields (FIELDS), keyword END
*/

/* DIMEN_SPECS */
 /* nonempty list of dimension specifications (DIMEN_SPEC)
    separated with commas */

/* DIMEN_SPEC */
/* left square bracket, constant factor (CONST_FACTOR), range operator,
   constant factor, right square  bracket */

/* FIELDS */
/* Possibly empty sequence of fields (FIELD), followed each by a semicolon */

/* FIELD */
/* Identifier list (IDENT_LIST), followed by a colon, and type specification
   (TYPE_SPEC) */

/* PROC_DECL */
/* Procedure header, semicolon, block, and identifier */

/* PROC_HEAD */
/* keyword PROCEDURE (KW_PROCEDURE), identifier (IDENT),
   optional formal parameters (OPT_FORMAL_PARAMS) */

/* OPT_FORMAL_PARAMS */
/* empty or formal parameters (FORMAL_PARAMS) */

/* FORMAL_PARAMS */
/* left parenthesis, formal parameter sections (FP_SECTIONS),
   right parenthesis, optional return type (OPT_RET_TYPE) */

/* FP_SECTIONS */
/* nonempty list of formal parameter sections (FP_SECTION) separated with
   semicolons */

/* FP_SECTION */
/* optional keyword VAR (OPT_VAR), identifier list (IDENT_LIST),
   colon, and qualified indentifier (QUALIDENT) */

/* OPT_VAR */
/* empty or keyword VAR (KW_VAR) */

/* OPT_RET_TYPE */
/* empty or colon and qualified identifier (QUALIDENT) */

/* QUALIDENT */
/* nonempty identifier list separated with dots */

/* STATEMENTS */
/* List of statements (STATEMENT) separated with semicolons */

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

/* ASSIGNMENT */
/* Identifier, qualifier assignment operator (ASSIGN), and expression (EXPR) */

/* QUALIF */
/* can either be empty,
   or a left square bracket, indexes (SUBSCRIPTS), right square  bracket,
      and qualifier,
   or a dot, identifier, and qualifier */

/* SUBSCRIPTS */
 /* nonepmty expression list separated with commas */

/* PROCEDURE_CALL */
/* Either only identifier,
 or identifier, left parenthesis, actual parameters (ACT_PARAMETERS),
 and right parenthesis */

/* ACT_PARAMETERS */
/* List of expressions (EXPR) separated with commas */

/* EXPR */
/* Either a simple expression (SIMPLE_EXPR),
   or two simple expressions separated with a relational operator (REL_OP) */

/* REL_OP */
/* Either equal sign, less, greater, hash, not equal (NEQ),
   less or equal (LE), greater or equal (GE), or keyword IN */

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

/* FACTOR */
/* Either interger constant, real constant, string constant,
   or character constant,
   identifier with a qualifier,
   procedure call, expression in parentheses,
   or a factor following either keyword NOT, or a unary minus
   (with NEG precedence) */

/* FOR_STATEMENT */
 /* Sequence: keyword FOR, identifier, assignment operator (ASSIGN),
    expression, TO or DOWNTO (TO_DOWNTO), expression, keyword DO,
   statements (STATEMENTS), and keyword END */

/* TO_DOWNTO */
 /* Either keyword TO, or keyword DOWNTO */

/* IF_STATEMENT */
/* Sequence: keyword IF, expression,
   keyword THEN, statements (STATEMENTS),  ELSIF part (ELSIFS),
   ELSE part (ELSE_PART), and keyword END */

/* ELSIFS */
/* Possibly empty sequence of sequences of the form:
   keyword ELSIF, expression, keyword THEN, and statements */

/* ELSE_PART */
/* Either empty or keyword ELSE followed by statements */

/* WHILE_STATEMENT */
/* keyword WHILE (KW_WHILE), expression (EXPR), keyword DO (KW_DO),
   statements (STATEMENTS), and keyword END (KW_END) */

/* REPEAT_STATEMENT */
/* keyword REPEAT (KW_REPEAT), statements (STATEMENTS),
   keyword UNTIL (KW_UNTIL), and expression (EXPR) */

/* LOOP_STATEMENT */
/* keyword LOOP (KW_LOOP), statements (STATEMENTS), and keyword end (KW_END) */

/* CASE_STATEMENT */
/* keyword CASE (KW_CASE), expression (EXPR), keyword OF,
   cases (CASES), ELSE part (ELSE_PART), and keyword END */

/* CASES */
/* lists of 3 items: CASE labels (CASE_LABELS), colon,
   and statements (STATEMENTS), every 3 items separated with vertical bars */

/* CASE_LABELS */
/* list of CASE_LABEL separated with commas */

/* CASE_LABEL */
/* Either an integer constant, or a character constant */

%%

int main( void )
{ 
	printf( "First and last name\n" );
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
