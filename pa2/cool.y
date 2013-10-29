/*
*  cool.y
*              Parser definition for the COOL language.
*
*/
%{
  #include <iostream>
  #include "cool-tree.h"
  #include "stringtab.h"
  #include "utilities.h"
  
  extern char *curr_filename;
  
  
  /* Locations */
  #define YYLTYPE int              /* the type of locations */
  #define cool_yylloc curr_lineno  /* use the curr_lineno from the lexer
  for the location of tokens */
    
    extern int node_lineno;          /* set before constructing a tree node
    to whatever you want the line number
    for the tree node to be */
      
      
      #define YYLLOC_DEFAULT(Current, Rhs, N)         \
      Current = Rhs[1];                             \
      node_lineno = Current;
    
    
    #define SET_NODELOC(Current)  \
    node_lineno = Current;
    
    /* IMPORTANT NOTE ON LINE NUMBERS
    *********************************
    * The above definitions and macros cause every terminal in your grammar to 
    * have the line number supplied by the lexer. The only task you have to
    * implement for line numbers to work correctly, is to use SET_NODELOC()
    * before constructing any constructs from non-terminals in your grammar.
    * Example: Consider you are matching on the following very restrictive 
    * (fictional) construct that matches a plus between two integer constants. 
    * (SUCH A RULE SHOULD NOT BE  PART OF YOUR PARSER):
    
    plus_consts	: INT_CONST '+' INT_CONST 
    
    * where INT_CONST is a terminal for an integer constant. Now, a correct
    * action for this rule that attaches the correct line number to plus_const
    * would look like the following:
    
    plus_consts	: INT_CONST '+' INT_CONST 
    {
      // Set the line number of the current non-terminal:
      // ***********************************************
      // You can access the line numbers of the i'th item with @i, just
      // like you acess the value of the i'th exporession with $i.
      //
      // Here, we choose the line number of the last INT_CONST (@3) as the
      // line number of the resulting expression (@$). You are free to pick
      // any reasonable line as the line number of non-terminals. If you 
      // omit the statement @$=..., bison has default rules for deciding which 
      // line number to use. Check the manual for details if you are interested.
      @$ = @3;
      
      
      // Observe that we call SET_NODELOC(@3); this will set the global variable
      // node_lineno to @3. Since the constructor call "plus" uses the value of 
      // this global, the plus node will now have the correct line number.
      SET_NODELOC(@3);
      
      // construct the result node:
      $$ = plus(int_const($1), int_const($3));
    }
    
    */
    
    
    
    void yyerror(char *s);        /*  defined below; called for each parse error */
    extern int yylex();           /*  the entry point to the lexer  */

    class let_stmt_exp_class : public Expression_class {
    public:
       Symbol identifier;
       Symbol type_decl;
       Expression init;
       let_stmt_exp_class(Symbol a1, Symbol a2, Expression a3) {
          identifier = a1;
          type_decl = a2;
          init = a3;
       }
       Expression copy_Expression(){
            return new let_stmt_exp_class(copy_Symbol(identifier), copy_Symbol(type_decl), init->copy_Expression());
       }
       void dump(ostream& stream, int n){
            stream << pad(n) << "let_stmt_exp\n";
            dump_Symbol(stream, n+2, identifier);
            dump_Symbol(stream, n+2, type_decl);
            init->dump(stream, n+2);
       }
       void dump_with_types(ostream& stream, int n){
            dump(stream, n);
       }
    };

    Expression create_let_stmt_exp(Symbol identifier, Symbol type_decl, Expression init)
    {
      return new let_stmt_exp_class(identifier, type_decl, init);
    }
    
    /************************************************************************/
    /*                DONT CHANGE ANYTHING IN THIS SECTION                  */
    
    Program ast_root;	      /* the result of the parse  */
    Classes parse_results;        /* for use in semantic analysis */
    int omerrs = 0;               /* number of errors in lexing and parsing */
    %}
    
    /* A union of all the types that can be the result of parsing actions. */
    %union {
      Boolean boolean;
      Symbol symbol;
      Program program;
      Class_ class_;
      Classes classes;
      Feature feature;
      Features features;
      Formal formal;
      Formals formals;
      Case case_;
      Cases cases;
      Expression expression;
      Expressions expressions;
      char *error_msg;
    }
    
    /* 
    Declare the terminals; a few have types for associated lexemes.
    The token ERROR is never used in the parser; thus, it is a parse
    error when the lexer returns it.
    
    The integer following token declaration is the numeric constant used
    to represent that token internally.  Typically, Bison generates these
    on its own, but we give explicit numbers to prevent version parity
    problems (bison 1.25 and earlier start at 258, later versions -- at
    257)
    */
    %token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
    %token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
    %token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
    %token <symbol>  STR_CONST 275 INT_CONST 276 
    %token <boolean> BOOL_CONST 277
    %token <symbol>  TYPEID 278 OBJECTID 279 
    %token ASSIGN 280 NOT 281 LE 282 ERROR 283
    
    /*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
    /**************************************************************************/
    
    /* Complete the nonterminal list below, giving a type for the semantic
    value of each non terminal. (See section 3.6 in the bison 
    documentation for details). */

    /* Declare types for the grammar's non-terminals. */
    %type <program> program
    %type <classes> class_list
    %type <class_> a_class
    %type <features> feature_list
    %type <feature> a_feature
    %type <formals> formal_list
    %type <formal> a_formal
    %type <cases> case_list
    %type <case_> a_case
    %type <expressions> exp_list_dispatch exp_list_block
    %type <expression> a_exp a_exp_compare a_exp_no_compare
    %type <expression> let_stmt let_stmt_exp
    %type <expressions> let_stmt_exp_list
    
    /* Precedence declarations go here. */
    %right ASSIGN IN
    %left '<' '=' LE
    %left '+' '-'
    %left '*' '/'
    %left '.'
    %left '@'
    %right ISVOID NOT '~' 
    
    %%
    /* 
    Save the root of the abstract syntax tree in a global variable.
    */
    program	: class_list	{ @$ = @1; ast_root = program($1); }
    ;
    
    class_list
    : a_class			/* single class */
    { 
      $$ = single_Classes($1);
      parse_results = $$;
    }
    | class_list a_class	/* several classes */
    { 
      $$ = append_Classes($1, single_Classes($2)); 
      parse_results = $$; 
    }
    | class_list error
    {}
    ;
    
    /* If no parent is specified, the class inherits from the Object class. */
    a_class
    : CLASS TYPEID '{' feature_list '}' ';'
    { 
      $$ = class_($2,idtable.add_string("Object"),$4,
      stringtable.add_string(curr_filename)); 
    }
    | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
    {
      $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); 
    }
    | CLASS error ';'
    {}
    
    /* Feature list may be empty, but no empty features in list. */
    feature_list
    :   /* empty */
    { $$ = nil_Features(); }
    | feature_list a_feature ';' /* several features */
    { $$ = append_Features($1, single_Features($2)); }
    | feature_list error ';'
    {}
    ;
    
    a_feature
    : OBJECTID '(' formal_list ')' ':' TYPEID '{' a_exp '}'
    { $$ = method($1, $3, $6, $8); }
    | OBJECTID ':' TYPEID
    { $$ = attr($1, $3, no_expr()); }
    | OBJECTID ':' TYPEID ASSIGN a_exp
    { $$ = attr($1, $3, $5); }
    ;

    formal_list
    : /* empty */
    { $$ = nil_Formals(); }
    | a_formal
    { $$ = single_Formals($1); }
    | formal_list ',' a_formal
    { $$ = append_Formals($1, single_Formals($3)); }
    ;
    
    a_formal
    : OBJECTID ':' TYPEID
    { $$ = formal($1, $3); }
    ;

    a_exp
    : a_exp_compare
    { $$ = $1; }
    | a_exp_no_compare
    { $$ = $1; }
    ;

    a_exp_no_compare
    : OBJECTID ASSIGN a_exp
    { $$ = assign($1, $3); }
    | a_exp '.' OBJECTID '(' exp_list_dispatch ')'
    { $$ = dispatch($1, $3, $5); }
    | a_exp '@' TYPEID '.' OBJECTID '(' exp_list_dispatch ')'
    { $$ = static_dispatch($1, $3, $5, $7); }
    | OBJECTID '(' exp_list_dispatch ')'
    { $$ = dispatch(object(idtable.add_string("self")), $1, $3); }
    | IF a_exp THEN a_exp ELSE a_exp FI
    { $$ = cond($2, $4, $6); }
    | WHILE a_exp LOOP a_exp POOL
    { $$ = loop($2, $4); }
    | '{' exp_list_block '}'
    { $$ = block($2); }
    | let_stmt
    { $$ = $1; }
    | CASE a_exp OF case_list ESAC
    { $$ = typcase($2, $4); }
    | NEW TYPEID
    { $$ = new_($2); }
    | ISVOID a_exp
    { $$ = isvoid($2); }
    | a_exp '+' a_exp
    { $$ = plus($1, $3); }
    | a_exp '-' a_exp
    { $$ = sub($1, $3); }
    | a_exp '*' a_exp
    { $$ = mul($1, $3); }
    | a_exp '/' a_exp
    { $$ = divide($1, $3); }
    | '~' a_exp
    { $$ = neg($2); }
    | NOT a_exp
    { $$ = comp($2); }
    | '(' a_exp ')'
    { $$ = $2; }
    | OBJECTID
    { $$ = object($1); }
    | INT_CONST
    { $$ = int_const($1); }
    | STR_CONST
    { $$ = string_const($1); }
    | BOOL_CONST
    { $$ = bool_const($1); }
    | '{' error '}'
    {}
    ;

    a_exp_compare
    : a_exp_no_compare '<' a_exp_no_compare
    { $$ = lt($1, $3); }
    | a_exp_no_compare LE a_exp_no_compare
    { $$ = leq($1, $3); }
    | a_exp_no_compare '=' a_exp_no_compare
    { $$ = eq($1, $3); }
    ;

    exp_list_dispatch
    : /* empty */
    { $$ = nil_Expressions(); }
    | a_exp
    { $$ = single_Expressions($1); }
    | exp_list_dispatch ',' a_exp
    { $$ = append_Expressions($1, single_Expressions($3)); }
    | exp_list_dispatch ',' error
    {}
    ;

    exp_list_block
    : a_exp ';'
    { $$ = single_Expressions($1); }
    | exp_list_block a_exp ';'
    { $$ = append_Expressions($1, single_Expressions($2)); }
    | exp_list_block error ';'
    | error ';' exp_list_block
    {}
    ;

    let_stmt
    : LET let_stmt_exp_list IN a_exp
    {
      Expressions l = $2;
      $$ = $4;
      for(int i = l->len() - 1; i >= 0; i--)
      {
        let_stmt_exp_class* lsec = (let_stmt_exp_class*)l->nth(i);
        Symbol identifier = lsec->identifier;
        Symbol type_decl = lsec->type_decl;
        Expression init = lsec->init;
        $$ = let(identifier, type_decl, init, $$);
      }
    }
    ;

    let_stmt_exp
    : OBJECTID ':' TYPEID
    { $$ = create_let_stmt_exp($1, $3, no_expr()); }
    | OBJECTID ':' TYPEID ASSIGN a_exp
    { $$ = create_let_stmt_exp($1, $3, $5); }
    ;

    let_stmt_exp_list
    : let_stmt_exp
    { $$ = single_Expressions($1); }
    | let_stmt_exp_list ',' let_stmt_exp
    { $$ = append_Expressions($1, single_Expressions($3)); }
    | let_stmt_exp_list ',' error
    | error
    {}
    ;

    case_list
    : a_case
    { $$ = single_Cases($1); }
    | case_list a_case
    { $$ = append_Cases($1, single_Cases($2)); }
    ;

    a_case
    : OBJECTID ':' TYPEID DARROW a_exp ';'
    { $$ = branch($1, $3, $5); }
    ;

    /* end of grammar */
    %%
    
    /* This function is called automatically when Bison detects a parse error. */
    void yyerror(char *s)
    {
      extern int curr_lineno;
      
      cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
      << s << " at or near ";
      print_cool_token(yychar);
      cerr << endl;
      omerrs++;
      
      if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
    }
    
    