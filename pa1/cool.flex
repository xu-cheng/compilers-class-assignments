/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

char error_strnullchar[] = "String contants null character";
char error_strtoolong[] = "String contant too long";
char error_streof[] = "EOF in string constant";
char error_strunend[] = "Unterminated string constant";
char error_commenteof[] = "EOF in comment";
char error_unmatchstar[] = "Unmatched *)";

#define putcharinstringbuf(c) \
  { \
    char tmp = c; \
    if ( tmp != '\0') { \ 
      *string_buf_ptr++ = tmp; \
      str_buf_len ++ ; \
    } \
    else { \
      brokenstring = true; \
      cool_yylval.error_msg = error_strnullchar; \
      return(ERROR); \
    } \
    \
    if ( str_buf_len == MAX_STR_CONST ) { \
      brokenstring = true; \
      cool_yylval.error_msg = error_strtoolong; \
      return(ERROR); \
    } \
  }

int comment_nest = 0;
int str_buf_len = 0;
bool brokenstring = false;

%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>
ASSIGN          <-
LE              <=
DIGIT          [0-9]
OBJECTID       [a-z][a-zA-Z0-9_]*
TYPEID         [A-Z][a-zA-Z0-9_]*
CLASS          [Cc][Ll][Aa][Ss][Ss]
ELSE           [Ee][Ll][Ss][Ee]
FI             [Ff][Ii]
IF             [Ii][Ff]
IN             [Ii][Nn]
INHERITS       [Ii][Nn][Hh][Ee][Rr][Ii][Tt][Ss]
LET            [Ll][Ee][Tt]
LOOP           [Ll][Oo][Oo][Pp]
POOL           [Pp][Oo][Oo][Ll]
THEN           [Tt][Hh][Ee][Nn]
WHILE          [Ww][Hh][Ii][Ll][Ee]
CASE           [Cc][Aa][Ss][Ee]
ESAC           [Ee][Ss][Aa][Cc]
OF             [Oo][Ff]
NEW            [Nn][Ee][Ww]
ISVOID         [Ii][Ss][Vv][Oo][Ii][Dd]
NOT            [Nn][Oo][Tt]
TRUE           t[Rr][Uu][Ee]
FALSE          f[Aa][Ll][Ss][Ee]

%x str
%x comment

%%

 /*
  *  Nested comments
  */


 /* The multiple-character operators. */

{DARROW}    { return (DARROW); }
{ASSIGN}    { return (ASSIGN); }
{LE}        { return (LE); }

 /* integer */
{DIGIT}+    { cool_yylval.symbol = inttable.add_string(yytext); return(INT_CONST); }

 /* keywords */
{CLASS}     { return(CLASS); }
{ELSE}      { return(ELSE); }
{FI}        { return(FI); }
{IF}        { return(IF); }
{IN}        { return(IN); }
{INHERITS}  { return(INHERITS); }
{LET}       { return(LET); }
{LOOP}      { return(LOOP); }
{POOL}      { return(POOL); }
{THEN}      { return(THEN); }
{WHILE}     { return(WHILE); }
{CASE}      { return(CASE); }
{ESAC}      { return(ESAC); }
{OF}        { return(OF); }
{NEW}       { return(NEW); }
{ISVOID}    { return(ISVOID); }
{NOT}       { return(NOT); }
{TRUE}      { cool_yylval.boolean = true; return(BOOL_CONST); }
{FALSE}     { cool_yylval.boolean = false; return(BOOL_CONST); }

 /* id */
{OBJECTID}  { cool_yylval.symbol = idtable.add_string(yytext); return(OBJECTID); }
{TYPEID}    { cool_yylval.symbol = idtable.add_string(yytext); return(TYPEID); }

 /* string */
"\""        { string_buf_ptr = string_buf;  memset(string_buf, 0 ,MAX_STR_CONST); str_buf_len = 0; BEGIN(str); }
<str><<EOF>> { BEGIN(INITIAL);  cool_yylval.error_msg = error_streof; return(ERROR); }
<str>"\""   { 
               BEGIN(INITIAL);
               if(brokenstring) {
                  brokenstring = false;
               } else {
                 *string_buf_ptr = '\0'; 
                 cool_yylval.symbol = stringtable.add_string(string_buf); 
                 return(STR_CONST); 
               }
            }
<str>\\[ \t\f\r\v]*\n  { curr_lineno++; putcharinstringbuf('\n'); }
<str>"\n"   { 
              curr_lineno++;
              BEGIN(INITIAL);
              *string_buf_ptr = '\0';
              cool_yylval.symbol = stringtable.add_string(string_buf);
              cool_yylval.error_msg = error_strunend; 
              return(ERROR);
            }
<str>\\n    { putcharinstringbuf('\n'); }
<str>\\t    { putcharinstringbuf('\t'); }
<str>\\b    { putcharinstringbuf('\b'); }
<str>\\f    { putcharinstringbuf('\f'); }
<str>\\[^ntbf]    { putcharinstringbuf(yytext[1]); }
<str>[^\\\n\"\0]+   {
                    char *yptr = yytext;
                    while ( *yptr ) {
                         putcharinstringbuf(*yptr++);      
                    }
                  }

 /* comments */
--.*
--.*$
"(*"       { BEGIN(comment); comment_nest++; }
<comment><<EOF>> { BEGIN(INITIAL); cool_yylval.error_msg = error_commenteof; return(ERROR); }
<comment>"(*"    { comment_nest++; }
<comment>\n      { curr_lineno++; }
<comment>.
<comment>"*)" { if(--comment_nest == 0) BEGIN(INITIAL); }
"*)"       { cool_yylval.error_msg = error_unmatchstar; return(ERROR); }

 /* single character*/
"."         { return((int)('.')); }
"@"         { return((int)('@')); }
"~"         { return((int)('~')); }
"*"         { return((int)('*')); }
"/"         { return((int)('/')); }
"+"         { return((int)('+')); }
"-"         { return((int)('-')); }
"<"         { return((int)('<')); }
"="         { return((int)('=')); }
"{"         { return((int)('{')); }
"}"         { return((int)('}')); }
"("         { return((int)('(')); }
")"         { return((int)(')')); }
":"         { return((int)(':')); }
";"         { return((int)(';')); }
","         { return((int)(',')); }

 /* eat up whitespace */
[ \t\f\r\v]+
"\n"        { curr_lineno++; }
.           { cool_yylval.error_msg = yytext; return(ERROR); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */


%%
