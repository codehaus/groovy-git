grammar groovy3;

options
{
    k = 2;
    output = AST; 
}

tokens {
	BLOCK; MODIFIERS; OBJBLOCK; SLIST; METHOD_DEF; VARIABLE_DEF;
	INSTANCE_INIT; STATIC_INIT; TYPE; CLASS_DEF; INTERFACE_DEF;
    PACKAGE_DEF; ARRAY_DECLARATOR; EXTENDS_CLAUSE; IMPLEMENTS_CLAUSE;
    PARAMETERS; PARAMETER_DEF; LABELED_STAT; TYPECAST; INDEX_OP;
    POST_INC; POST_DEC; METHOD_CALL; EXPR;
    IMPORT; UNARY_MINUS; UNARY_PLUS; CASE_GROUP; ELIST; FOR_INIT; FOR_CONDITION;
    FOR_ITERATOR; EMPTY_STAT; FINAL='final'; ABSTRACT='abstract';
    UNUSED_GOTO='goto'; UNUSED_CONST='const'; UNUSED_DO='do';
    STRICTFP='strictfp'; SUPER_CTOR_CALL; CTOR_CALL; CTOR_IDENT; VARIABLE_PARAMETER_DEF;
    STRING_CONSTRUCTOR; STRING_CTOR_MIDDLE;
    CLOSABLE_BLOCK; IMPLICIT_PARAMETERS;
    SELECT_SLOT; DYNAMIC_MEMBER;
    LABELED_ARG; SPREAD_ARG; SPREAD_MAP_ARG; //deprecated - SCOPE_ESCAPE;
    LIST_CONSTRUCTOR; MAP_CONSTRUCTOR;
    FOR_IN_ITERABLE;
    STATIC_IMPORT; ENUM_DEF; ENUM_CONSTANT_DEF; FOR_EACH_CLAUSE; ANNOTATION_DEF; ANNOTATIONS;
    ANNOTATION; ANNOTATION_MEMBER_VALUE_PAIR; ANNOTATION_FIELD_DEF; ANNOTATION_ARRAY_INIT;
    TYPE_ARGUMENTS; TYPE_ARGUMENT; TYPE_PARAMETERS; TYPE_PARAMETER; WILDCARD_TYPE;
    TYPE_UPPER_BOUNDS; TYPE_LOWER_BOUNDS; CLOSURE_LIST;MULTICATCH;MULTICATCH_TYPES;
}

//To disable generation of the standard exception handling code in the parser
@rulecatch { } 

//Header for lexer
@lexer::header {
   package org.codehaus.groovy.antlr.parser;
   
	import org.codehaus.groovy.antlr.*;
	import java.util.*;
	import java.io.InputStream;
	import java.io.Reader;
	import antlr.InputBuffer;
	import antlr.LexerSharedInputState;
	import antlr.CommonToken;
	import org.codehaus.groovy.GroovyBugError;
	import antlr.TokenStreamRecognitionException;
}

//Header for parser
@parser::header {
	package org.codehaus.groovy.antlr.parser;
   
	import org.codehaus.groovy.antlr.*;
	import java.util.*;
	import java.io.InputStream;
	import java.io.Reader;
	import antlr.InputBuffer;
	import antlr.LexerSharedInputState;
	import antlr.CommonToken;
	import org.codehaus.groovy.GroovyBugError;
	import antlr.TokenStreamRecognitionException;
}

@lexer::members {
	private Stack<String> paraphrase = new Stack<String>();
	
	 /** flag for enabling the "assert" keyword */
    private boolean assertEnabled = true;
    /** flag for enabling the "enum" keyword */
    private boolean enumEnabled = true;
    /** flag for including whitespace tokens (for IDE preparsing) */
    private boolean whitespaceIncluded = false;

    /** Enable the "assert" keyword */
    public void enableAssert(boolean shouldEnable) { assertEnabled = shouldEnable; }
    /** Query the "assert" keyword state */
    public boolean isAssertEnabled() { return assertEnabled; }
    /** Enable the "enum" keyword */
    public void enableEnum(boolean shouldEnable) { enumEnabled = shouldEnable; }
    /** Query the "enum" keyword state */
    public boolean isEnumEnabled() { return enumEnabled; }

    /** Include whitespace tokens.  Note that this breaks the parser.   */
    public void setWhitespaceIncluded(boolean z) { whitespaceIncluded = z; }
    /** Are whitespace tokens included? */
    public boolean isWhitespaceIncluded() { return whitespaceIncluded; }

    {
        // Initialization actions performed on construction.
        setTabSize(1);  // get rid of special tab interpretation, for IDEs and general clarity
    }

    /** Bumped when inside '[x]' or '(x)', reset inside '{x}'.  See ONE_NL.  */
    protected int parenLevel = 0;
    protected int suppressNewline = 0;  // be really mean to newlines inside strings
    protected static final int SCS_TYPE = 3, SCS_VAL = 4, SCS_LIT = 8, SCS_LIMIT = 16;
    protected static final int SCS_SQ_TYPE = 0, SCS_TQ_TYPE = 1, SCS_RE_TYPE = 2, SCS_DRE_TYPE = 3;
    protected int stringCtorState = 0;  // hack string and regexp constructor boundaries
    /** Push parenLevel here and reset whenever inside '{x}'. */
    protected ArrayList parenLevelStack = new ArrayList();
    protected int lastSigTokenType = EOF;  // last returned non-whitespace token

    public void setTokenObjectClass(String name) {/*ignore*/}
    
    
}

@members {	
	 // Scratch variable for last 'sep' token.
    // Written by the 'sep' rule, read only by immediate callers of 'sep'.
    // (Not entirely clean, but better than a million xx=sep occurrences.)
    private int sepToken = EOF;

    // Scratch variable for last argument list; tells whether there was a label.
    // Written by 'argList' rule, read only by immediate callers of 'argList'.
    private boolean argListHasLabels = false;

    // Scratch variable, holds most recently completed pathExpression.
    // Read only by immediate callers of 'pathExpression' and 'expression'.
    private AST lastPathExpression = null;

    // Inherited attribute pushed into most expression rules.
    // If not zero, it means that the left context of the expression
    // being parsed is a statement boundary or an initializer sign '='.
    // Only such expressions are allowed to reach across newlines
    // to pull in an LCURLY and appended block.
    private final int LC_STMT = 1, LC_INIT = 2;

    /**
     * Counts the number of LT seen in the typeArguments production.
     * It is used in semantic predicates to ensure we have seen
     * enough closing '>' characters; which actually may have been
     * either GT, SR or BSR tokens.
     */
    private int ltCounter = 0;

    /* This symbol is used to work around a known ANTLR limitation.
     * In a loop with syntactic predicate, ANTLR needs help knowing
     * that the loop exit is a second alternative.
     * Example usage:  ( (LCURLY)=> block | {ANTLR_LOOP_EXIT}? )*
     * Probably should be an ANTLR RFE.
     */
    ////// Original comment in Java grammar:
    // Unfortunately a syntactic predicate can only select one of
    // multiple alternatives on the same level, not break out of
    // an enclosing loop, which is why this ugly hack (a fake
    // empty alternative with always-false semantic predicate)
    // is necessary.
    private static final boolean ANTLR_LOOP_EXIT = false;
    
    public void addWarning(String warning, String solution) {
        Token lt = null;
        try { lt = LT(1); }
        catch (TokenStreamException ee) { }
        if (lt == null)  lt = Token.badToken;

        Map row = new HashMap();
        row.put("warning",  warning);
        row.put("solution", solution);
        row.put("filename", getFilename());
        row.put("line",     Integer.valueOf(lt.getLine()));
        row.put("column",   Integer.valueOf(lt.getColumn()));
        // System.out.println(row);
        warningList.add(row);
    }
}


/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/

// modifiers for Java classes, interfaces, class/instance vars and methods
modifier
    :   'private'
    |   'public'
    |   'protected'
    |   'static'
    |   'transient'
    |   'final'
    |   'abstract'
    |   'native'
    |   'threadsafe'
    |   'synchronized'
    |   'volatile'
    |   'strictfp'
    ;    

// The primitive types.
builtInType
    :   'void'
    |   'boolean'
    |   'byte'
    |   'char'
    |   'short'
    |   'int'
    |   'float'
    |   'long'
    |   'double'
    ;
    

/*
 * Allowed keywords after dot (as a member name) and before colon (as a label).
 * Includes all Java keywords plus "in" and "as".
 */
keywordPropertyNames
    :   (
          'as'
        | 'assert'
        | 'break'
        | 'case'
        | 'catch'
        | 'class'
        | 'const'
        | 'continue'
        | 'def'
        | 'default'
        | 'do'
        | 'else'
        | 'enum'
        | 'extends'
        | 'false'
        | 'finally'
        | 'for'
        | 'goto'
        | 'if'
        | 'implements'
        | 'import'
        | 'in'
        | 'instanceof'
        | 'interface'
        | 'new'
        | 'null'
        | 'package'
        | 'return'
        | 'super'
        | 'switch'
        | 'this'
        | 'throw'
        | 'throws'
        | 'true'
        | 'try'      
        | 'while'
        | modifier
        | builtInType
        )
        { #keywordPropertyNames.setType(IDENT); }
    ;

/** Used in cases where a declaration cannot have commas, or ends with the "in" operator instead of '='. */
singleVariable[AST mods, AST t]  
	@init {Token first = LT(1);}
    :
        id=variableName
        {#singleVariable = #(create(VARIABLE_DEF,"VARIABLE_DEF",first,LT(1)), mods, #(create(TYPE,"TYPE",first,LT(1)),t), id);}
    ;

variableName
    :   IDENT
    ;
    
/** Declaration of a variable. This can be a class/instance variable,
 *  or a local variable in a method
 *  It can also include possible initialization.
 */
variableDeclarator[AST mods, AST t,Token first]
    :
        id=variableName
        /*OBS*d:declaratorBrackets[t]*/
        (v=varInitializer)?
        {#variableDeclarator = #(create(VARIABLE_DEF,"VARIABLE_DEF",first,LT(1)), mods, #(create(TYPE,"TYPE",first,LT(1)),t), id, v);}
    ;    

listOfVariables[AST mods, AST t, Token first]
    :
        variableDeclarator[getASTFactory().dupTree(mods),
                           getASTFactory().dupTree(t),first]
        (   COMMA! nls!
            {first = LT(1);}
            variableDeclarator[getASTFactory().dupTree(mods),
                               getASTFactory().dupTree(t),first]
        )*
    ;
    
// A (possibly-qualified) java identifier. We start with the first IDENT
// and expand its name by adding dots and following IDENTS
identifier 
	@init {Token first = LT(1);}
    :   i1=IDENT!
        (   options { greedy = true; } :
            d=DOT! nls! i2=IDENT!
            {$i1 = #(create(DOT,".",first,LT(1)),$i1,$i2);}
        )*
        {$identifier = $i1;}
    ;

identifierStar 
	@init {Token first = LT(1);}
    :   i1=IDENT!
        (   options { greedy = true; } :
            d1=DOT! nls! i2=IDENT!
            {$i1 = #(create(DOT,".",first,LT(1)),$i1,$i2);}
        )*
        (   d2=DOT!  nls! s=STAR!
            {$i1 = #(create(DOT,".",first,LT(1)),$i1,$s);}
        |   'as'! nls! alias=IDENT!
            {$i1 = #(create(LITERAL_as,"as",first,LT(1)),$i1,$alias);}
        )?
        {$identifierStar = $i1;}
    ;

modifiersInternal
    @init    { int seenDef = 0; }
    :
        (
            // Without this hush, there is a warning that @IDENT and @interface
            // can follow modifiersInternal.  But how is @IDENT possible after
            // modifiersInternal?  And how is @interface possible inside modifiersInternal?
            // Is there an antlr bug?
            //options{generateAmbigWarnings=false;}:

            // 'def' is an empty modifier, for disambiguating declarations
            {seenDef++ == 0}?       // do not allow multiple "def" tokens
            'def'! nls!
        |
            // Note: Duplication of modifiers is detected when walking the AST.
            modifier nls!
        |
            {break; /* go out of the ()+ loop*/}
            AT 'interface'
        |
            annotation nls!
        )+
    ;

/** A list of one or more modifier, annotation, or "def". */
modifiers  
	@init {Token first = LT(1);}
    :   modifiersInternal
        {#modifiers = #(create(MODIFIERS, "MODIFIERS",first,LT(1)), #modifiers);}
    ;

/** A list of zero or more modifiers, annotations, or "def". */
modifiersOpt  
	@init {Token first = LT(1);}
    :   (
            // See comment above on hushing warnings.
            //options{generateAmbigWarnings=false;}:
            modifiersInternal
        )?
        {#modifiersOpt = #(create(MODIFIERS, "MODIFIERS",first,LT(1)), #modifiersOpt);}
    ;        
    
annotation/*!*/
	@init {Token first = LT(1);}
    :   AT! i=identifier nls! (options{greedy=true;}: LPAREN! ( args=annotationArguments )? RPAREN! )?
        {$annotation = #(create(ANNOTATION,"ANNOTATION",first,LT(1)), $i, $args);}
    ;

annotationsInternal
    :   (
//            options{generateAmbigWarnings=false;}:
            {break; /* go out of the ()* loop*/}
            AT 'interface'
        |
            annotation nls!)*
    ;

annotationsOpt  
	@init {Token first = LT(1);}
    :   (
               // See comment above on hushing warnings.
               //options{generateAmbigWarnings=false;}:
               annotationsInternal
        )?
        {$annotationsOpt = $(create(ANNOTATIONS, "ANNOTATIONS", first, LT(1)), $annotationsOpt);}
    ;

annotationArguments
    :   v=annotationMemberValueInitializer
            { Token itkn = new Token(IDENT, "value");
              AST i;
              $i = #(create(IDENT, "value", itkn, itkn));
              $annotationArguments = #(create(ANNOTATION_MEMBER_VALUE_PAIR,"ANNOTATION_MEMBER_VALUE_PAIR",LT(1),LT(1)), i, $v);}
            | annotationMemberValuePairs
    ;

annotationMemberValuePairs
    :   annotationMemberValuePair ( COMMA! nls! annotationMemberValuePair )*
    ;

annotationMemberValuePair/*!*/
	@init   {Token first = LT(1);}
    :   i=annotationIdent ASSIGN! nls! v=annotationMemberValueInitializer
            {$annotationMemberValuePair = #(create(ANNOTATION_MEMBER_VALUE_PAIR,"ANNOTATION_MEMBER_VALUE_PAIR",first,LT(1)), $i, $v);}
    ;

annotationIdent
    :   IDENT
    |   keywordPropertyNames
    ;

annotationMemberValueInitializer
    :   conditionalExpression[0] | annotation
    ;

/*OBS*
// This is an initializer used to set up an annotation member array.
annotationMemberArrayInitializer
    :   lc:LCURLY^ {#lc.setType(ANNOTATION_ARRAY_INIT);}
        (   annotationMemberArrayValueInitializer
            (
                // CONFLICT: does a COMMA after an initializer start a new
                // initializer or start the option ',' at end?
                // ANTLR generates proper code by matching
                // the comma as soon as possible.
                options {
                        warnWhenFollowAmbig = false;
                }
            :
                COMMA! nls! annotationMemberArrayValueInitializer
            )*
            (COMMA! nls!)?
        )?
        RCURLY!
    ;
*OBS*/

// The two things that can initialize an annotation array element are a conditional expression
// and an annotation (nested annotation array initialisers are not valid)
annotationMemberArrayValueInitializer
    :   conditionalExpression[0]
    |   annotation nls!
    ;                     

/** An expression may be followed by one or both of (...) and {...}.
 *  Note: If either is (...) or {...} present, it is a method call.
 *  The {...} is appended to the argument list, and matches a formal of type Closure.
 *  If there is no method member, a property (or field) is used instead, and must itself be callable.
 *  <p>
 *  If the methodCallArgs are absent, it is a property reference.
 *  If there is no property, it is treated as a field reference, but never a method reference.
 *  <p>
 *  Arguments in the (...) can be labeled, and the appended block can be labeled also.
 *  If there is a mix of unlabeled and labeled arguments,
 *  all the labeled arguments must follow the unlabeled arguments,
 *  except that the closure (labeled or not) is always a separate final argument.
 *  Labeled arguments are collected up and passed as a single argument to a formal of type Map.
 *  <p>
 *  Therefore, f(x,y, a:p, b:q) {s} is equivalent in all ways to f(x,y, [a:p,b:q], {s}).
 *  Spread arguments of sequence type count as unlabeled arguments,
 *  while spread arguments of map type count as labeled arguments.
 *  (This distinction must sometimes be checked dynamically.)
 *
 *  A plain unlabeled argument is allowed to match a trailing Map or Closure argument:
 *  f(x, a:p) {s}  ===  f(*[ x, [a:p], {s} ])
 */
// AST is [METHOD_CALL, callee, ELIST? CLOSABLE_BLOCK?].
// Note that callee is often of the form x.y but not always.
// If the callee is not of the form x.y, then an implicit .call is needed.
// Parameter callee is only "null" when called from newExpression
methodCallArgs[AST callee]
    :
        LPAREN!
        al=argList!
        RPAREN!
        { if (callee != null && callee.getFirstChild() != null) {
              //method call like obj.method()
              #methodCallArgs = #(create(METHOD_CALL, "(",callee.getFirstChild(),LT(1)), callee, al);
          } else {
              //method call like method() or new Expr(), in the latter case "callee" is null
              #methodCallArgs = #(create(METHOD_CALL, "(",callee, LT(1)), callee, al);
          }
        }
    ;

/** An appended block follows any expression.
 *  If the expression is not a method call, it is given an empty argument list.
 */
appendedBlock[AST callee]
    :
        /*  FIXME DECIDE: should appended blocks accept labels?
        (   (IDENT COLON nls LCURLY)=>
            IDENT c:COLON^ {#c.setType(LABELED_ARG);} nls!
        )? */
        cb=closableBlock!
        {
            // If the callee is itself a call, flatten the AST.
            if (callee != null && callee.getType() == METHOD_CALL) {
                #appendedBlock = #(create(METHOD_CALL, "(",callee,LT(1)),
                                   callee.getFirstChild(), cb);
            } else {
                #appendedBlock = #(create(METHOD_CALL, "{",callee,LT(1)), callee, cb);
            }
        }
    ;

// *TODO* We must also audit the various occurrences of warning
// suppressions like "options { greedy = true; }".

/** A declaration with one declarator and no initialization, like a parameterDeclaration.
 *  Used to parse loops like <code>for (int x in y)</code> (up to the <code>in</code> keyword).
 */
singleDeclarationNoInit!
    :
        // method/variable using a 'def' or a modifier; type is optional
        m=modifiers
        (t=typeSpec[false])?
        v=singleVariable[#m, #t]
        {#singleDeclarationNoInit = #v;}
    |
        // method/variable using a type only
        t2=typeSpec[false]
        v2=singleVariable[null,#t2]
        {#singleDeclarationNoInit = #v2;}
    ;

/** A declaration with one declarator and optional initialization, like a parameterDeclaration.
 *  Used to parse declarations used for both binding and effect, in places like argument
 *  lists and <code>while</code> statements.
 */
singleDeclaration
    :   sd=singleDeclarationNoInit!
        { #singleDeclaration = #sd; }
        (varInitializer)?
    ;

/** Used only as a lookahead predicate, before diving in and parsing a declaration.
 *  A declaration can be unambiguously introduced with "def", an annotation or a modifier token like "final".
 *  It may also be introduced by a simple identifier whose first character is an uppercase letter,
 *  as in {String x}.  A declaration can also be introduced with a built in type like 'int' or 'void'.
 *  Brackets (array and generic) are allowed, as in {List[] x} or {int[][] y}.
 *  Anything else is parsed as a statement of some sort (expression or command).
 *  <p>
 *  (In the absence of explicit method-call parens, we assume a capitalized name is a type name.
 *  Yes, this is a little hacky.  Alternatives are to complicate the declaration or command
 *  syntaxes, or to have the parser query the symbol table.  Parse-time queries are evil.
 *  And we want both {String x} and {println x}.  So we need a syntactic razor-edge to slip
 *  between 'println' and 'String'.)
 *
 *   *TODO* The declarationStart production needs to be strengthened to recognize
 *  things like {List<String> foo}.
 *  Right now it only knows how to skip square brackets after the type, not
 *  angle brackets.
 *  This probably turns out to be tricky because of >> vs. > >. If so,
 *  just put a TODO comment in.
 */
declarationStart!
    :   (     ('def' nls)
            | modifier nls
            | annotation nls
            | (   upperCaseIdent
                |   builtInType
                |   qualifiedTypeName
              ) (typeArguments)? (LBRACK balancedTokens RBRACK)*
        )+
        ( IDENT | STRING_LITERAL )
    ;

qualifiedTypeName!
    :
        IDENT DOT (IDENT DOT)* upperCaseIdent
    ;

/** An assignment operator '=' followed by an expression.  (Never empty.) */
varInitializer
    :   ASSIGN^ nls! expressionStatementNoCheck
        // In {T x = y}, the left-context of y is that of an initializer.
    ;

/** A list of zero or more formal parameters.
 *  If a parameter is variable length (e.g. String... myArg) it should be
 *  to the right of any other parameters of the same kind.
 *  General form:  (req, ..., opt, ..., [rest], key, ..., [restKeys], [block]
 *  This must be sorted out after parsing, since the various declaration forms
 *  are impossible to tell apart without backtracking.
 */
parameterDeclarationList  
	@init {Token first = LT(1);}
    :
        (
            parameterDeclaration
            (   COMMA! nls!
                parameterDeclaration
            )*
        )?
        {#parameterDeclarationList = #(create(PARAMETERS,"PARAMETERS",first,LT(1)),
                                       #parameterDeclarationList);}
    ;

/** A formal parameter for a method or closure. */
parameterDeclaration/*!*/
    @init { Token first = LT(1);boolean spreadParam = false; }
    :
        pm=parameterModifiersOpt
        (   options {greedy=true;} :
            t=typeSpec[false]
        )?

        // TODO:  What do formal parameters for keyword arguments look like?

        // Java-style var args
        ( TRIPLE_DOT! { spreadParam = true; } )?

        id=IDENT

        // allow an optional default value expression
        (exp=varInitializer)?

        /*OBS*pd:declaratorBrackets[#t]*/
        {
            if (spreadParam) {
                #parameterDeclaration = #(create(VARIABLE_PARAMETER_DEF,"VARIABLE_PARAMETER_DEF",first,LT(1)),
                      pm, #(create(TYPE,"TYPE",first,LT(1)),t), id, exp);
            } else {
                #parameterDeclaration = #(create(PARAMETER_DEF,"PARAMETER_DEF",first,LT(1)),
                      pm, #(create(TYPE,"TYPE",first,LT(1)),t), id, exp);
            }
        }
    ;

parameterModifiersOpt
    @init    { Token first = LT(1);int seenDef = 0; }
        //final and/or def can appear amongst annotations in any order
    :   (   {seenDef++ == 0}?       // do not allow multiple "def" tokens
            'def'!  nls!            // redundant, but allowed for symmetry
        |   'final' nls!
        |   annotation nls!
        )*
        {#parameterModifiersOpt = #(create(MODIFIERS,"MODIFIERS",first,LT(1)), #parameterModifiersOpt);}
    ;
    
/** Closure parameters are exactly like method parameters,
 *  except that they are not enclosed in parentheses, but rather
 *  are prepended to the front of a block, just after the brace.
 *  They are separated from the closure body by a CLOSABLE_BLOCK_OP token '->'.
 */
// With '|' there would be restrictions on bitwise-or expressions.
closableBlockParamsOpt[boolean addImplicit]
    :   (parameterDeclarationList nls CLOSABLE_BLOCK_OP)=>
        parameterDeclarationList nls! CLOSABLE_BLOCK_OP! nls!
    |   {addImplicit}?
        implicitParameters
    |
        /* else do not parse any parameters at all */
    ;

/** Lookahead to check whether a block begins with explicit closure arguments. */
closableBlockParamsStart!
    :
        nls parameterDeclarationList nls CLOSABLE_BLOCK_OP
    ;

/** Simple names, as in {x|...}, are completely equivalent to {(def x)|...}.  Build the right AST. */
closableBlockParam/*!*/  
	@init {Token first = LT(1);}
    :   id=IDENT!
        {#closableBlockParam = #(create(PARAMETER_DEF,"PARAMETER_DEF",first,LT(1)),
                               #(create(MODIFIERS,"MODIFIERS",first,LT(1))), #(create(TYPE,"TYPE",first,LT(1))),
                               id);}
    ;
  
// Compound statement. This is used in many contexts:
// Inside a class definition prefixed with "static":
// it is a class initializer
// Inside a class definition without "static":
// it is an instance initializer
// As the body of a method
// As a completely independent braced block of code inside a method
// it starts a new scope for variable definitions
// In Groovy, this is called an "open block".  It cannot have closure arguments.

compoundStatement
    :   openBlock
    ;

/** An open block is not allowed to have closure arguments. */
openBlock  
	@init {Token first = LT(1);}
    :   LCURLY! nls!
        // AST type of SLIST means "never gonna be a closure"
        bb=blockBody[EOF]!
        RCURLY!
        {#openBlock = #(create(SLIST,"{",first,LT(1)),bb);}

    ;

/** A block body is a parade of zero or more statements or expressions. */
blockBody[int prevToken]
    :
        //TODO:
    ;
            
/** A block which is known to be a closure, even if it has no apparent arguments.
 *  A block inside an expression or after a method call is always assumed to be a closure.
 *  Only labeled, unparameterized blocks which occur directly as substatements are kept open.
 */
closableBlock  
	@init {Token first = LT(1);}
    :   LCURLY! nls!
        cbp=closableBlockParamsOpt[true]!
        bb=blockBody[EOF]!
        RCURLY!
        {#closableBlock = #(create(CLOSABLE_BLOCK,"{",first,LT(1)),cbp,bb);}
    ;
    

stringConstructorValuePart
    :
    (   identifier
    |   'this' |   'super'
    |   openOrClosableBlock
    )
    ;    

/** A member name (x.y) or element name (x[y]) can serve as a command name,
 *  which may be followed by a list of arguments.
 *  Unlike parenthesized arguments, these must be plain expressions,
 *  without labels or spread operators.
 */
commandArguments[AST head]
@init {
	Token first = LT(1);
}
    :
        commandArgument ( options {greedy=true;}: COMMA! nls! commandArgument )*
        // println 2+2 //OK
        // println(2+2) //OK
        // println (2)+2 //BAD
        // println((2)+2) //OK
        // (println(2)+2) //OK
        // compare (2), 2 //BAD
        // compare( (2), 2 ) //OK
        // foo.bar baz{bat}, bang{boz} //OK
        {
            AST elist = #(create(ELIST,"ELIST",first,LT(1)), #commandArguments);
            AST headid = #(create(METHOD_CALL,"<command>",first,LT(1)), head, elist);
            #commandArguments = headid;
        }
    ;

commandArgumentsGreedy[AST head]
@init { 
	AST prev = null;
}
    :
        { #prev = #head; }
        
        // argument to the already existing method name
        (   ({#prev.getType()!=METHOD_CALL}? commandArgument)=> (   
                first=commandArguments[head]!
                { #prev = #first; }
            )
            |
        )
        
        // we start a series of methods and arguments
        (   options { greedy = true; } :
            (   options { greedy = true; } :
                // method name
                pre=primaryExpression!
                { #prev = #(create(DOT, ".", #prev), #prev, #pre); }
                // what follows is either a normal argument, parens, 
                // an appended block, an index operation, or nothing
                // parens (a b already processed): 
                //      a b c() d e -> a(b).c().d(e)
                //      a b c()() d e -> a(b).c().call().d(e)
                // index (a b already processed): 
                //      a b c[x] d e -> a(b).c[x].d(e)
                //      a b c[x][y] d e -> a(b).c[x][y].d(e)
                // block (a b already processed):
                //      a b c {x} d e -> a(b).c({x}).d(e)
                //
                // parens/block completes method call
                // index makes method call to property get with index
                // 
                (options {greedy=true;}:
                (pathElementStart)=>   
                    (   
                        pc=pathChain[LC_STMT,#prev]!
                        { #prev = #pc; }
                    )      
                |
                    (   ca=commandArguments[#prev]!
                        { #prev = #ca; })
                )?
            )*
        )
        { #commandArgumentsGreedy = prev; } 
    ;

commandArgument
    :
        (argumentLabel COLON nls!) => (
            argumentLabel c=COLON^ nls! expression[0]  { #c.setType(LABELED_ARG); }
        )
        | expression[0]
    ;
    
// expressions
// Note that most of these expressions follow the pattern
//   thisLevelExpression :
//         nextHigherPrecedenceExpression
//                 (OPERATOR nextHigherPrecedenceExpression)*
// which is a standard recursive definition for a parsing an expression.
// The operators in java have the following precedences:
//      lowest  ( 15)  = **= *= /= %= += -= <<= >>= >>>= &= ^= |=
//              ( 14)  ?: (conditional expression and elvis)
//              ( 13)  ||
//              ( 12)  &&
//              ( 11)  |
//              ( 10)  ^
//              (  9)  &
//              (8.5)  =~ ==~
//              (  8)  == != <=> === !==
//              (  7)  < <= > >= instanceof as in
//              (  6)  << >> .. ..<
//              (  5)  +(binary) -(binary)
//              (  4)  * / %
//              (  3)  **(power)
//              (  2)  ++(pre) --(pre) +(unary) -(unary)
//              (  1)  ~  ! $ (type) ++(post) --(post)
//                     . ?. *. (dot -- identifier qualification)
//                     []   () (method call)  {} (closableBlock)  [] (list/map)
//                     new  () (explicit parenthesis)
//                     $x (scope escape)
//
// the last two are not usually on a precedence chart; I put them in
// to point out that new has a higher precedence than '.', so you
// can validly use
//       new Frame().show()
//
// Note that the above precedence levels map to the rules below...
// Once you have a precedence chart, writing the appropriate rules as below
// is usually very straightforward


// the mother of all expressions
// This nonterminal is not used for expression statements, which have a more restricted syntax
// due to possible ambiguities with other kinds of statements.  This nonterminal is used only
// in contexts where we know we have an expression.  It allows general Java-type expressions.
expression[int lc_stmt]
    :
        (LPAREN typeSpec[true] RPAREN expression[lc_stmt])=>
            lp=LPAREN^ {#lp.setType(TYPECAST);} typeSpec[true] RPAREN!
            expression[lc_stmt]
    |
       (LPAREN nls IDENT (COMMA nls IDENT)* RPAREN ASSIGN) =>
        m=multipleAssignment[lc_stmt] {#expression=#m;}
    |   assignmentExpression[lc_stmt]
    ;

multipleAssignment[int lc_stmt] 
	@init {Token first = cloneToken(LT(1));}
    :   LPAREN^ nls! listOfVariables[null,null,first] RPAREN!
        ASSIGN^ nls!
        assignmentExpression[lc_stmt]
    ;    

/** A sub-block of a block can be either open or closable.
 *  It is closable if and only if there are explicit closure arguments.
 *  Compare this to a block which is appended to a method call,
 *  which is given closure arguments, even if they are not explicit in the code.
 */
openOrClosableBlock  
	@init {Token first = LT(1);}
    :   LCURLY! nls!
        cp=closableBlockParamsOpt[false]!
        bb=blockBody[EOF]!
        RCURLY!
        {
            if (#cp == null)    #openOrClosableBlock = #(create(SLIST,"{",first,LT(1)),bb);
            else                #openOrClosableBlock = #(create(CLOSABLE_BLOCK,"{",first,LT(1)),cp,bb);
        }
    ;

/** A block known to be a closure, but which omits its arguments, is given this placeholder.
 *  A subsequent pass is responsible for deciding if there is an implicit 'it' parameter,
 *  or if the parameter list should be empty.
 */
implicitParameters  
	@init {Token first = LT(1);}
    :   {   #implicitParameters = #(create(IMPLICIT_PARAMETERS,"IMPLICIT_PARAMETERS",first,LT(1)));  }
    ;

/** If a dot is followed by a parenthesized or quoted expression, the member is computed dynamically,
 *  and the member selection is done only at runtime.  This forces a statically unchecked member access.
 */
dynamicMemberName  
	@init {Token first = LT(1);}
    :   (   pe=parenthesizedExpression!
            {#dynamicMemberName = #(create(EXPR,"EXPR",first,LT(1)),pe);}
        |   // TODO: stringConstructorExpression
        )
        { #dynamicMemberName = #(create(DYNAMIC_MEMBER, "DYNAMIC_MEMBER",first,LT(1)), #dynamicMemberName); }
    ;

/** An expression may be followed by [...].
 *  Unlike Java, these brackets may contain a general argument list,
 *  which is passed to the array element operator, which can make of it what it wants.
 *  The brackets may also be empty, as in T[].  This is how Groovy names array types.
 *  <p>Returned AST is [INDEX_OP, indexee, ELIST].
 */
indexPropertyArgs[AST indexee]
    :
        lb=LBRACK
        al=argList!
        RBRACK!
        { if (indexee != null && indexee.getFirstChild() != null) {
              //expression like obj.index[]
              #indexPropertyArgs = #(create(INDEX_OP, "INDEX_OP",indexee.getFirstChild(),LT(1)), lb, indexee, al);
          } else {
              //expression like obj[]
              #indexPropertyArgs = #(create(INDEX_OP, "INDEX_OP",indexee,LT(1)), lb, indexee, al);
          }
        }
    ;

// assignment expression (level 15)
assignmentExpression[int lc_stmt]
    :   conditionalExpression[lc_stmt]
        (
            (   ASSIGN^
            |   PLUS_ASSIGN^
            |   MINUS_ASSIGN^
            |   STAR_ASSIGN^
            |   DIV_ASSIGN^
            |   MOD_ASSIGN^
            |   SR_ASSIGN^
            |   BSR_ASSIGN^
            |   SL_ASSIGN^
            |   BAND_ASSIGN^
            |   BXOR_ASSIGN^
            |   BOR_ASSIGN^
            |   STAR_STAR_ASSIGN^
            //|   USEROP_13^  //DECIDE: This is how user-define ops would show up.
            )
            nls!
            expressionStatementNoCheck
            // If left-context of {x = y} is a statement boundary,
            // define the left-context of y as an initializer.
        )?
    ;

// conditional test (level 14)
conditionalExpression[int lc_stmt]
    :   logicalOrExpression[lc_stmt]
        (
          (ELVIS_OPERATOR)=> ELVIS_OPERATOR^ nls! conditionalExpression[0]
          | QUESTION^ nls! assignmentExpression[0] COLON! nls! conditionalExpression[0]
        )?
    ;


// logical or (||)  (level 13)
logicalOrExpression[int lc_stmt]
    :   logicalAndExpression[lc_stmt] (LOR^ nls! logicalAndExpression[0])*
    ;


// logical and (&&)  (level 12)
logicalAndExpression[int lc_stmt]
    :   inclusiveOrExpression[lc_stmt] (LAND^ nls! inclusiveOrExpression[0])*
    ;

// bitwise or non-short-circuiting or (|)  (level 11)
inclusiveOrExpression[int lc_stmt]
    :   exclusiveOrExpression[lc_stmt] (BOR^ nls! exclusiveOrExpression[0])*
    ;


// exclusive or (^)  (level 10)
exclusiveOrExpression[int lc_stmt]
    :   andExpression[lc_stmt] (BXOR^ nls! andExpression[0])*
    ;


// bitwise or non-short-circuiting and (&)  (level 9)
andExpression[int lc_stmt]
    :   regexExpression[lc_stmt] (BAND^ nls! regexExpression[0])*
    ;

// regex find and match (=~ and ==~) (level 8.5)
// jez: moved =~ closer to precedence of == etc, as...
// 'if (foo =~ "a.c")' is very close in intent to 'if (foo == "abc")'
regexExpression[int lc_stmt]
    :   equalityExpression[lc_stmt] ((REGEX_FIND^ | REGEX_MATCH^) nls! equalityExpression[0])*
    ;

// equality/inequality (==/!=) (level 8)
equalityExpression[int lc_stmt]
    :   relationalExpression[lc_stmt] ((NOT_EQUAL^ | EQUAL^ |IDENTICAL^ |NOT_IDENTICAL^ | COMPARE_TO^) nls! relationalExpression[0])*
    ;

// boolean relational expressions (level 7)
relationalExpression[int lc_stmt]
    :   shiftExpression[lc_stmt]
        (   options {greedy=true;} : (
                (   LT^
                |   GT^
                |   LE^
                |   GE^
                |   'in'^
                )
                nls!
                shiftExpression[0]

            )
            |   'instanceof'^ nls! typeSpec[true]
            |   'as'^         nls! typeSpec[true] //TODO: Rework to allow type expression?
        )?
    ;



// bit shift expressions (level 6)
shiftExpression[int lc_stmt]
    :   additiveExpression[lc_stmt]
        (
            ((SL^ | SR^ | BSR^)
            |   RANGE_INCLUSIVE^
            |   RANGE_EXCLUSIVE^
            )
            nls!
            additiveExpression[0]
        )*
    ;


// binary addition/subtraction (level 5)
additiveExpression[int lc_stmt]
    :   multiplicativeExpression[lc_stmt]
        (
            options {greedy=true;} :
            // Be greedy here, to favor {x+y} instead of {print +value}
            (PLUS^ | MINUS^) nls!
            multiplicativeExpression[0]
        )*
    ;


// multiplication/division/modulo (level 4)
multiplicativeExpression[int lc_stmt]
    :    ( INC^ nls!  powerExpressionNotPlusMinus[0] ((STAR^ | DIV^ | MOD^ )  nls!  powerExpression[0])* )
    |    ( DEC^ nls!  powerExpressionNotPlusMinus[0] ((STAR^ | DIV^ | MOD^ )  nls!  powerExpression[0])* )
    |    ( MINUS^ {#MINUS.setType(UNARY_MINUS);} nls!   powerExpressionNotPlusMinus[0] ((STAR^ | DIV^ | MOD^ )  nls!  powerExpression[0])* )
    |    ( PLUS^ {#PLUS.setType(UNARY_PLUS);} nls!   powerExpressionNotPlusMinus[0] ((STAR^ | DIV^ | MOD^ )  nls!  powerExpression[0])* )
    |    (  powerExpressionNotPlusMinus[lc_stmt] ((STAR^ | DIV^ | MOD^ )  nls!  powerExpression[0])* )
    ;

// math power operator (**) (level 3)
powerExpression[int lc_stmt]
    :   unaryExpression[lc_stmt] (STAR_STAR^ nls! unaryExpression[0])*
    ;

// math power operator (**) (level 3)
// (without ++(prefix)/--(prefix)/+(unary)/-(unary))
// The different rules are needed to avoid ambiguous selection
// of alternatives.
powerExpressionNotPlusMinus[int lc_stmt]
    :   unaryExpressionNotPlusMinus[lc_stmt] (STAR_STAR^ nls! unaryExpression[0])*
    ;

// ++(prefix)/--(prefix)/+(unary)/-(unary) (level 2)
unaryExpression[int lc_stmt]
    :   INC^ nls! unaryExpression[0]
    |   DEC^ nls! unaryExpression[0]
    |   MINUS^   {#MINUS.setType(UNARY_MINUS);}   nls! unaryExpression[0]
    |   PLUS^    {#PLUS.setType(UNARY_PLUS);}     nls! unaryExpression[0]
    |   unaryExpressionNotPlusMinus[lc_stmt]
    ;

// ~(BNOT)/!(LNOT)/(type casting) (level 1)
unaryExpressionNotPlusMinus[int lc_stmt]
    :   //BAND^    {#BAND.setType(MEMBER_POINTER_DEFAULT);}   nls!  namePart
    //|
        BNOT^ nls! unaryExpression[0]
    |   LNOT^ nls! unaryExpression[0]
    |   (   // subrule allows option to shut off warnings
            //options {
                    // "(int" ambig with postfixExpr due to lack of sequence
                    // info in linear approximate LL(k). It's ok. Shut up.
                    //generateAmbigWarnings=false;
            //}
        :   // If typecast is built in type, must be numeric operand
            // Have to backtrack to see if operator follows
            // FIXME: DECIDE: This syntax is wormy.  Can we deprecate or remove?
            (LPAREN builtInTypeSpec[true] RPAREN unaryExpression[0])=>
            lpb=LPAREN^ {#lpb.setType(TYPECAST);} builtInTypeSpec[true] RPAREN!
            unaryExpression[0]

            // Have to backtrack to see if operator follows. If no operator
            // follows, it's a typecast. No semantic checking needed to parse.
            // if it _looks_ like a cast, it _is_ a cast; else it's a "(expr)"
            // FIXME: DECIDE: This syntax is wormy.  Can we deprecate or remove?
            // TODO:  Rework this mess for Groovy.
        |   (LPAREN classTypeSpec[true] RPAREN unaryExpressionNotPlusMinus[0])=>
            lp=LPAREN^ {#lp.setType(TYPECAST);} classTypeSpec[true] RPAREN!
            unaryExpressionNotPlusMinus[0]

        |   postfixExpression[lc_stmt]
        )
    ;

// qualified names, array expressions, method invocation, post inc/dec
postfixExpression[int lc_stmt]
    :
        pathExpression[lc_stmt]
        (
            options {greedy=true;} :
            // possibly add on a post-increment or post-decrement.
            // allows INC/DEC on too much, but semantics can check
            in=INC^ {#in.setType(POST_INC);}
        |   de=DEC^ {#de.setType(POST_DEC);}
        )?
    ;

// TODO:  Move pathExpression to this point in the file.

// the basic element of an expression
primaryExpression 
	@init {Token first = LT(1);}
    :   IDENT
        /*OBS*  //keywords can follow dot in Groovy; no need for this special case
        ( options {greedy=true;} : DOT^ "class" )?
        *OBS*/
    |   constant
    |   //TODO: newExpression
    |   'this'
    |   'super'
    |   pe=parenthesizedExpression!             // (general stuff...)
        {#primaryExpression = #(create(EXPR,"EXPR",first,LT(1)),pe);}
    |   //TODO: closableBlockConstructorExpression
    |   listOrMapConstructorExpression
    |   //TODO: stringConstructorExpression         // "foo $bar baz"; presented as multiple tokens
//deprecated    |   scopeEscapeExpression               // $x
    |   builtInType
    /*OBS*  //class names work fine as expressions
            // look for int.class and int[].class
    |   bt:builtInType!
        declaratorBrackets[bt]
        DOT^ nls! "class"
    *OBS*/
    ;

// Note:  This is guaranteed to be an EXPR AST.
// That is, parentheses are preserved, in case the walker cares about them.
// They are significant sometimes, as in (f(x)){y} vs. f(x){y}.
parenthesizedExpression
@init {   Token first = LT(1);
    Token declaration = null;
    boolean hasClosureList=false;
    boolean firstContainsDeclaration=false;
    boolean sce=false;
}
    :   LPAREN!
           { declaration=LT(1); }
           firstContainsDeclaration = strictContextExpression[true]
           (SEMI!
             {hasClosureList=true;}
             (sce=strictContextExpression[true] | { astFactory.addASTChild(currentAST,astFactory.create(EMPTY_STAT, "EMPTY_STAT")); })
           )*
           // if the first expression contained a declaration,
           // but we are having only one expression at all, then
           // the first declaration is of the kind (def a=b)
           // which is invalid. Therefore if there was no closure
           // list we let the compiler throw an error if the
           // the first declaration exists
           {
            if (firstContainsDeclaration && !hasClosureList)
               throw new NoViableAltException(declaration, getFilename());
           }
        RPAREN!
        {
            if (hasClosureList) {
                #parenthesizedExpression = #(create(CLOSURE_LIST,"CLOSURE_LIST",first,LT(1)),#parenthesizedExpression);
            }
        }
    ;

/** Things that can show up as expressions, but only in strict
 *  contexts like inside parentheses, argument lists, and list constructors.
 */
strictContextExpression[boolean allowDeclaration]
returns [boolean hasDeclaration=false]
@init {Token first = LT(1);}
    :
        (   ({allowDeclarpathChaination}? declarationStart)=>
            {hasDeclaration=true;} singleDeclaration  // used for both binding and value, as: while (String xx = nextln()) { println xx }
        |   expression[0]
        |   //TODO: branchStatement // useful to embed inside expressions (cf. C++ throw)
        |   annotation      // creates an annotation value
        )
        // For the sake of the AST walker, mark nodes like this very clearly.
        {#strictContextExpression = #(create(EXPR,"EXPR",first,LT(1)),#strictContextExpression);}
    ;

assignmentLessExpression  
	@init {Token first = LT(1);}
    :
        (   conditionalExpression[0]
        )
        // For the sake of the AST walker, mark nodes like this very clearly.
        {#assignmentLessExpression = #(create(EXPR,"EXPR",first,LT(1)),#assignmentLessExpression);}
    ;

expressionStatementNoCheck
    @init { boolean isPathExpr = false; }
    :
        // Checks are now out of the way; here's the real rule:
        head=expression[LC_STMT]
        { isPathExpr = (#head == lastPathExpression); }
        (
            // A path expression (e.g., System.out.print) can take arguments.
            {LA(1)!=LITERAL_else && isPathExpr /*&& #head.getType()==METHOD_CALL*/}?
            cmd=commandArgumentsGreedy[#head]!
            {
                #expressionStatementNoCheck = #cmd;
            }
        )?
    ;



/**
 * A list constructor is a argument list enclosed in square brackets, without labels.
 * Any argument can be decorated with a spread operator (*x), but not a label (a:x).
 * Examples:  [], [1], [1,2], [1,*l1,2], [*l1,*l2].
 * (The l1, l2 must be a sequence or null.)
 * <p>
 * A map constructor is an argument list enclosed in square brackets, with labels everywhere,
 * except on spread arguments, which stand for whole maps spliced in.
 * A colon alone between the brackets also forces the expression to be an empty map constructor.
 * Examples: [:], [a:1], [a:1,b:2], [a:1,*:m1,b:2], [*:m1,*:m2]
 * (The m1, m2 must be a map or null.)
 * Values associated with identical keys overwrite from left to right:
 * [a:1,a:2]  ===  [a:2]
 * <p>
 * Some malformed constructor expressions are not detected in the parser, but in a post-pass.
 * Bad examples: [1,b:2], [a:1,2], [:1].
 * (Note that method call arguments, by contrast, can be a mix of keyworded and non-keyworded arguments.)
 */
// The parser allows a mix of labeled and unlabeled arguments, but there must be a semantic check that
// the arguments are all labeled (or SPREAD_MAP_ARG) or all unlabeled (and not SPREAD_MAP_ARG).
listOrMapConstructorExpression
    @init    { boolean hasLabels = false; }
    :   lcon=LBRACK!
        args=argList                 { hasLabels |= argListHasLabels;  }  // any argument label implies a map
        RBRACK!
        {   int type = hasLabels ? MAP_CONSTRUCTOR : LIST_CONSTRUCTOR;
            $listOrMapConstructorExpression = #(create(type,"[",lcon,LT(1)),args);
        }
    |
        /* Special case:  [:] is an empty map constructor. */
        emcon=LBRACK^  COLON! RBRACK! { #emcon.setType(MAP_CONSTRUCTOR) }
    ;

pathChain[int lc_stmt, AST prefix]
    :
        (
            options {
                // \n{foo} could match here or could begin a new statement
                // We do want to match here. Turn off warning.
                greedy=true;
                // This turns the ambiguity warning of the second alternative
                // off. See below. (The "ANTLR_LOOP_EXIT" predicate makes it non-issue)
                //@@ warnWhenFollowAmbig=false;
            }
            // Parsing of this chain is greedy.  For example, a pathExpression may be a command name
            // followed by a command argument, but that command argument cannot begin with an LPAREN,
            // since a parenthesized expression is greedily attached to the pathExpression as a method argument.
            // The lookahead is also necessary to reach across newline in foo \n {bar}.
            // (Apparently antlr's basic approximate LL(k) lookahead is too weak for this.)
        :   (pathElementStart)=>
            nls!
            pe=pathElement[prefix]!
            { prefix = #pe; }
        |
            {lc_stmt == LC_STMT || lc_stmt == LC_INIT}?
            (nls LCURLY)=>
            nlsWarn!
            apb=appendedBlock[prefix]!
            { prefix = #apb; }
        )+

        { #pathChain = prefix; }
    ;

/** A "path expression" is a name or other primary, possibly qualified by various
 *  forms of dot, and/or followed by various kinds of brackets.
 *  It can be used for value or assigned to, or else further qualified, indexed, or called.
 *  It is called a "path" because it looks like a linear path through a data structure.
 *  Examples:  x.y, x?.y, x*.y, x.@y; x[], x[y], x[y,z]; x(), x(y), x(y,z); x{s}; a.b[n].c(x).d{s}
 *  (Compare to a C lvalue, or LeftHandSide in the JLS section 15.26.)
 *  General expressions are built up from path expressions, using operators like '+' and '='.
 */
pathExpression[int lc_stmt]
    @init    { AST prefix = null; }
    :
        pre=primaryExpression!
        { prefix = #pre; }
        (
            options {
                // \n{foo} could match here or could begin a new statement
                // We do want to match here. Turn off warning.
                greedy=true;
                // This turns the ambiguity warning of the second alternative
                // off. See below. (The "ANTLR_LOOP_EXIT" predicate makes it non-issue)
                //@@ warnWhenFollowAmbig=false;
            }
            // Parsing of this chain is greedy.  For example, a pathExpression may be a command name
            // followed by a command argument, but that command argument cannot begin with an LPAREN,
            // since a parenthesized expression is greedily attached to the pathExpression as a method argument.
            // The lookahead is also necessary to reach across newline in foo \n {bar}.
            // (Apparently antlr's basic approximate LL(k) lookahead is too weak for this.)
        :   (pathElementStart)=>
            nls!
            pe=pathElement[prefix]!
            { prefix = #pe; }
        |
            {lc_stmt == LC_STMT || lc_stmt == LC_INIT}?
            (nls LCURLY)=>
            nlsWarn!
            apb=appendedBlock[prefix]!
            { prefix = #apb; }
        )*
        {
            #pathExpression = prefix;
            lastPathExpression = #pathExpression;
        }
    ;

pathElement[AST prefix] 
	@init {Token operator = LT(1);}
        // The primary can then be followed by a chain of .id, (a), [a], and {...}
    :
        { #pathElement = prefix; }
        (   // Spread operator:  x*.y  ===  x?.collect{it.y}
            SPREAD_DOT!
        |   // Optional-null operator:  x?.y  === (x==null)?null:x.y
            OPTIONAL_DOT!
        |   // Member pointer operator: foo.&y == foo.metaClass.getMethodPointer(foo, "y")
            MEMBER_POINTER!
        |   // The all-powerful dot.
            (nls! DOT!)
        ) nls!
        (ta=typeArguments!)?
        np=namePart!
    pathElementStart    { #pathElement = #(create(operator.getType(),operator.getText(),prefix,LT(1)),prefix,ta,np); }

    |
        mca=methodCallArgs[prefix]!
        {   #pathElement = #mca;  }
    |
        // Can always append a block, as foo{bar}
        apb=appendedBlock[prefix]!
        {   #pathElement = #apb;  }
    |
        // Element selection is always an option, too.
        // In Groovy, the stuff between brackets is a general argument list,
        // since the bracket operator is transformed into a method call.
        ipa=indexPropertyArgs[prefix]!
        {   #pathElement = #ipa;  }
/*    |
        (DOT nls "this") => DOT! nls! thisPart:"this"!
        { #pathElement = #(create(operator.getType(),operator.getText(),prefix,LT(1)),prefix,thisPart); }
/*NYI*
    |   DOT^ nls! "this"

    |   DOT^ nls! "super"
        (   // (new Outer()).super()  (create enclosing instance)
            lp3:LPAREN^ argList RPAREN!
            {#lp3.setType(SUPER_CTOR_CALL);}
        |   DOT^ IDENT
            (   lps:LPAREN^ {#lps.setType(METHOD_CALL);}
                argList
                RPAREN!declaratorBrackets
            )?
        )
    |   DOT^ nls! newExpression
*NYI*/
    ;

pathElementStart!
    :   (nls! DOT)
    |   SPREAD_DOT
    |   OPTIONAL_DOT
    |   MEMBER_POINTER
    |   LBRACK
    |   LPAREN
    |   LCURLY
    ;
    
/** This is the grammar for what can follow a dot:  x.a, x.@a, x.&a, x.'a', etc.
 *  Note: <code>typeArguments</code> is handled by the caller of <code>namePart</code>.
 */
namePart  
	@init {Token first = LT(1);}
    :
        (   ats=AT^     {#ats.setType(SELECT_SLOT);}  )?
        // foo.@bar selects the field (or attribute), not property

        (   IDENT
        |   sl=STRING_LITERAL {#sl.setType(IDENT);}
            // foo.'bar' is in all ways same as foo.bar, except that bar can have an arbitrary spelling
        |   dynamicMemberName
        |
            openBlock
            // PROPOSAL, DECIDE:  Is this inline form of the 'with' statement useful?
            // Definition:  a.{foo} === {with(a) {foo}}
            // May cover some path expression use-cases previously handled by dynamic scoping (closure delegates).

            // let's allow common keywords as property names
        |   keywordPropertyNames
        )

        // (No, x.&@y is not needed; just say x.&y as Slot or some such.)
    ;    

argList
    @init {
        Token first = LT(1);
        Token lastComma = null;
        int hls=0, hls2=0;
        boolean hasClosureList=false;
        boolean trailingComma=false;
        boolean sce=false;
    }
    :
        // Note:  nls not needed, since we are inside parens,
        // and those insignificant newlines are suppressed by the lexer.
        (hls=argument
        ((
            (
                SEMI! {hasClosureList=true;}
                (
                    sce=strictContextExpression[true]
                    | { astFactory.addASTChild(currentAST,astFactory.create(EMPTY_STAT, "EMPTY_STAT")); }
                )
            )+
            {#argList = #(create(CLOSURE_LIST,"CLOSURE_LIST",first,LT(1)),#argList);}
        ) | (
                (   {lastComma = LT(1);}
                    COMMA!
                    (
                        (hls2=argument {hls |= hls2;})
                        |
                        (
                         {  if (trailingComma) throw new NoViableAltException(lastComma, getFilename());
                            trailingComma=true;
                         }
                        )
                    )

                )*
                {#argList = #(create(ELIST,"ELIST",first,LT(1)), argList);}
            )
        ) | (
            {#argList = create(ELIST,"ELIST",first,LT(1));}
        )
        )
        {argListHasLabels = (hls&1)!=0; }
    ;
    
/** A single argument in (...) or [...].  Corresponds to to a method or closure parameter.
 *  May be labeled.  May be modified by the spread operator '*' ('*:' for keywords).
 */
argument
returns [byte hasLabelOrSpread = 0]
@init {boolean sce=false;}
    :
        // Optional argument label.
        // Usage:  Specifies a map key, or a keyworded argument.
        (   (argumentLabelStart) =>
            argumentLabel c=COLON^          {#c.setType(LABELED_ARG);}

            {   hasLabelOrSpread |= 1;  }  // signal to caller the presence of a label

        |   // Spread operator:  f(*[a,b,c])  ===  f(a,b,c);  f(1,*null,2)  ===  f(1,2).
            sp=STAR^                        {#sp.setType(SPREAD_ARG);}
            {   hasLabelOrSpread |= 2;  }  // signal to caller the presence of a spread operator
            // spread maps are marked, as f(*:m) for f(a:x, b:y) if m==[a:x, b:y]
            (
                COLON!                      {#sp.setType(SPREAD_MAP_ARG);}
                { hasLabelOrSpread |= 1; }  // signal to caller the presence of a label
            )?
        )?

        sce=strictContextExpression[true]
        {
            require(LA(1) != COLON,
                "illegal colon after argument expression",
                "a complex label expression before a colon must be parenthesized");
        }
    ;

/** A label for an argument is of the form a:b, 'a':b, "a":b, (a):b, etc..
 *      The labels in (a:b), ('a':b), and ("a":b) are in all ways equivalent,
 *      except that the quotes allow more spellings.
 *  Equivalent dynamically computed labels are (('a'):b) and ("${'a'}":b)
 *  but not ((a):b) or "$a":b, since the latter cases evaluate (a) as a normal identifier.
 *      Bottom line:  If you want a truly variable label, use parens and say ((a):b).
 */
argumentLabel
    :   (IDENT) =>
        id=IDENT                  {#id.setType(STRING_LITERAL);}  // identifiers are self-quoting in this context
    |   (keywordPropertyNames) =>
        kw=keywordPropertyNames   {#kw.setType(STRING_LITERAL);}  // identifiers are self-quoting in this context
    |   primaryExpression                                         // dynamic expression
    ;

/** For lookahead only.  Fast approximate parse of an argumentLabel followed by a colon. */
argumentLabelStart!
        // allow number and string literals as labels for maps
    :   (
            IDENT | keywordPropertyNames
        |   constantNumber | STRING_LITERAL
        |   (
        	LPAREN 
        	| STRING_CTOR_START
        	)=> balancedBrackets
        )
        COLON
    ;    
    
// A builtin type specification is a builtin type with possible brackets
// afterwards (which would make it an array type).
builtInTypeSpec[boolean addImagNode]  
	@init {Token first = LT(1);}
    :   bt=builtInType!
        declaratorBrackets[#bt]
        {
            if ( addImagNode ) {
                #builtInTypeSpec = #(create(TYPE,"TYPE",first,LT(1)), #builtInTypeSpec);
            }
        }
    ; 

/** Used only as a lookahead predicate for nested type declarations. */

/*TODO* The lookahead in typeDeclarationStart needs to skip annotations, not
just stop at '@', because variable and method declarations can also be
annotated.
> typeDeclarationStart!
>     :   (modifier!)* ("class" | "interface" | "enum" | AT )
S.B. something like
>     :   (modifier! | annotationTokens!)* ("class" | "interface" |
> "enum" )
(And maybe @interface, if Java 5 allows nested annotation types? Don't
know offhand.)
Where annotationTokens can be a quick paren-skipper, as in other
places: '@' ident '(' balancedTokens ')'.
*/

typeDeclarationStart!
    :   modifiersOpt! ('class' | 'interface' | 'enum' | AT 'interface')
    ;

/** An IDENT token whose spelling is required to start with an uppercase letter.
 *  In the case of a simple statement {UpperID name} the identifier is taken to be a type name, not a command name.
 */
upperCaseIdent
    :   {isUpperCase(LT(1))}?
        IDENT
    ;

// A type specification is a type name with possible brackets afterwards
// (which would make it an array type).
// Set addImagNode true for types inside expressions, not declarations.
typeSpec[boolean addImagNode]
    :    classTypeSpec[addImagNode]
    |    builtInTypeSpec[addImagNode]
    ;

// also check that 'classOrInterfaceType[false]' is a suitable substitution for 'identifier'

// A class type specification is a class type with either:
// - possible brackets afterwards
//   (which would make it an array type).
// - generic type arguments after
classTypeSpec[boolean addImagNode]  
	@init {Token first = LT(1);}
    :   ct=classOrInterfaceType[false]!
        declaratorBrackets[#ct]
        {
            if ( addImagNode ) {
                #classTypeSpec = #(create(TYPE,"TYPE",first,LT(1)), #classTypeSpec);
            }
        }
    ;

// A non-built in type name, with possible type parameters
classOrInterfaceType[boolean addImagNode] 
	@init {Token first = LT(1);}
    :   i1=IDENT^ (typeArguments|typeArgumentsDiamond)?

        (   options{greedy=true;}: // match as many as possible
            d=DOT!
            i2=IDENT! (ta=typeArguments!)?
            {#i1 = #(create(DOT,".",first,LT(1)),i1,i2,ta);}
        )*
        {
            #classOrInterfaceType = #i1;
            if ( addImagNode ) {
                #classOrInterfaceType = #(create(TYPE,"TYPE",first,LT(1)), #classOrInterfaceType);
            }
        }
    ;

// A specialised form of typeSpec where built in types must be arrays
typeArgumentSpec
    :   classTypeSpec[true]
    |   builtInTypeArraySpec[true]
    ;

// A generic type argument is a class type, a possibly bounded wildcard type or a built-in type array
typeArgument  
	@init {Token first = LT(1);}
    :   (   typeArgumentSpec
        |   wildcardType
        )
        {#typeArgument = #(create(TYPE_ARGUMENT,"TYPE_ARGUMENT",first,LT(1)), #typeArgument);}
    ;

// Wildcard type indicating all types (with possible constraint)
wildcardType
    :   QUESTION
        (('extends' | 'super')=> typeArgumentBounds)?
        {#wildcardType.setType(WILDCARD_TYPE);}
    ;

typeArgumentsDiamond
@init {Token first = LT(1);}
    :   LT! GT! nls!
    ;

// Type arguments to a class or interface type
typeArguments
@int {Token first = LT(1);
int currentLtLevel = 0;}
    :
        {currentLtLevel = ltCounter;}
        LT! {ltCounter++;} nls!
        typeArgument
        (   options{greedy=true;}: // match as many as possible
            {inputState.guessing !=0 || ltCounter == currentLtLevel + 1}?
            COMMA! nls! typeArgument
        )*
        nls!
        (   // turn warning off since Antlr generates the right code,
            // plus we have our semantic predicate below
            //options{generateAmbigWarnings=false;}:
            typeArgumentsOrParametersEnd
        )?

        // make sure we have gobbled up enough '>' characters
        // if we are at the "top level" of nested typeArgument productions
        {matchGenericTypeBrackets(((currentLtLevel != 0) || ltCounter == currentLtLevel),
        "Missing closing bracket '>' for generics types", "Please specify the missing bracket!")}?

        {#typeArguments = #(create(TYPE_ARGUMENTS, "TYPE_ARGUMENTS",first,LT(1)), #typeArguments);}
    ;

// this gobbles up *some* amount of '>' characters, and counts how many
// it gobbled.
fragment typeArgumentsOrParametersEnd
    :   GT! {ltCounter-=1;}
    |   SR! {ltCounter-=2;}
    |   BSR! {ltCounter-=3;}
    ;

// Restriction on wildcard types based on super class or derived class
typeArgumentBounds
    @init {Token first = LT(1);boolean isUpperBounds = false;}
    :
        ( 'extends'! {isUpperBounds=true;} | 'super'! ) nls! classOrInterfaceType[true] nls!
        {
            if (isUpperBounds)
            {
                #typeArgumentBounds = #(create(TYPE_UPPER_BOUNDS,"TYPE_UPPER_BOUNDS",first,LT(1)), #typeArgumentBounds);
            }
            else
            {
                #typeArgumentBounds = #(create(TYPE_LOWER_BOUNDS,"TYPE_LOWER_BOUNDS",first,LT(1)), #typeArgumentBounds);
            }
        }
    ;

// A builtin type array specification is a builtin type with brackets afterwards
builtInTypeArraySpec[boolean addImagNode]  
	@init {Token first = LT(1);}
    :   bt=builtInType!
        (   (LBRACK)=>   // require at least one []
            declaratorBrackets[#bt]
        |   {require(false,
                          "primitive type parameters not allowed here",
                           "use the corresponding wrapper type, such as Integer for int"
                           );}
        )
        {
            if ( addImagNode ) {
                #builtInTypeArraySpec = #(create(TYPE,"TYPE",first,LT(1)), #builtInTypeArraySpec);
            }
        }
    ;
    
/** After some type names, where zero or more empty bracket pairs are allowed.
 *  We use ARRAY_DECLARATOR to represent this.
 *  TODO:  Is there some more Groovy way to view this in terms of the indexed property syntax?
 */
declaratorBrackets[AST typ]
    :   {#declaratorBrackets=typ;}
        (
            // A following list constructor might conflict with index brackets; prefer the declarator.
            options {greedy=true;} :
            LBRACK!
            RBRACK!
            {#declaratorBrackets = #(create(ARRAY_DECLARATOR,"[",typ,LT(1)),
                               #declaratorBrackets);}
        )*
    ;
    
// A type name. which is either a (possibly qualified and parameterized)
// class name or a primitive (builtin) type
type
    :   classOrInterfaceType[false]
    |   builtInType
    ;       

/** Numeric, string, regexp, boolean, or null constant. */
constant
    :   constantNumber
    |   STRING_LITERAL
    |   'true'
    |   'false'
    |   'null'
    ;

/** Numeric constant. */
constantNumber
    :   NUM_INT
    |   NUM_FLOAT
    |   NUM_LONG
    |   NUM_DOUBLE
    |   NUM_BIG_INT
    |   NUM_BIG_DECIMAL
    ;

/** Fast lookahead across balanced brackets of all sorts. */
balancedBrackets!
    :   LPAREN balancedTokens RPAREN
    |   LBRACK balancedTokens RBRACK
    |   LCURLY balancedTokens RCURLY
    |   STRING_CTOR_START balancedTokens STRING_CTOR_END
    ;

balancedTokens!
    :   (   balancedBrackets
        |   ~(LPAREN|LBRACK|LCURLY | STRING_CTOR_START
             |RPAREN|RBRACK|RCURLY | STRING_CTOR_END)
        )*
    ;

/** A statement separator is either a semicolon or a significant newline.
 *  Any number of additional (insignificant) newlines may accompany it.
 */
//  (All the '!' signs simply suppress the default AST building.)
//  Returns the type of the separator in this.sepToken, in case it matters.
sep!
    :   SEMI!
        (options { greedy=true; }: NLS!)*
        { sepToken = SEMI; }
    |   NLS!                // this newline is significant!
        { sepToken = NLS; }
        (
            options { greedy=true; }:
            SEMI!           // this superfluous semicolon is gobbled
            (options { greedy=true; }: NLS!)*
            { sepToken = SEMI; }
        )*
    ;

/** Zero or more insignificant newlines, all gobbled up and thrown away. */
nls!
    :
        (options { greedy=true; }: NLS!)?
        // Note:  Use '?' rather than '*', relying on the fact that the lexer collapses
        // adjacent NLS tokens, always.  This lets the parser use its LL(3) lookahead
        // to "see through" sequences of newlines.  If there were a '*' here, the lookahead
        // would be weaker, since the parser would have to be prepared for long sequences
        // of NLS tokens.
    ;

/** Zero or more insignificant newlines, all gobbled up and thrown away,
 *  but a warning message is left for the user, if there was a newline.
 */
nlsWarn!
    :
        (   (NLS)=>
            { addWarning(
              "A newline at this point does not follow the Groovy Coding Conventions.",
              "Keep this statement on one line, or use curly braces to break across multiple lines."
            ); }
        )?
        nls!
    ;
 
 /*------------------------------------------------------------------
 * LEXER RULES
 *------------------------------------------------------------------*/

// OPERATORS
QUESTION          
    @init { paraphrase.push("?"); }
    @after { paraphrase.pop(); }
          :   '?'             ;
LPAREN            
    @init { paraphrase.push("("); }
    @after { paraphrase.pop(); }
          :   '('             {++parenLevel;};
RPAREN            
    @init { paraphrase.push(")"); }
    @after { paraphrase.pop(); }
          :   ')'             {--parenLevel;};
LBRACK            
    @init { paraphrase.push("["); }
    @after { paraphrase.pop(); }
          :   '['             {++parenLevel;};
RBRACK            
    @init { paraphrase.push("]"); }
    @after { paraphrase.pop(); }
          :   ']'             {--parenLevel;};
LCURLY            
    @init { paraphrase.push("{"); }
    @after { paraphrase.pop(); }
          :   '{'             {pushParenLevel();};
RCURLY            
    @init { paraphrase.push("}"); }
    @after { paraphrase.pop(); }
          :   '}'             {popParenLevel(); if(stringCtorState!=0) restartStringCtor(true);};
COLON             
    @init { paraphrase.push(":"); }
    @after { paraphrase.pop(); }
          :   ':'             ;
COMMA             
    @init { paraphrase.push(","); }
    @after { paraphrase.pop(); }
          :   ','             ;
DOT               
    @init { paraphrase.push("."); }
    @after { paraphrase.pop(); }
          :   '.'             ;
ASSIGN            
    @init { paraphrase.push("="); }
    @after { paraphrase.pop(); }
          :   '='             ;
COMPARE_TO        
    @init { paraphrase.push("<=>"); }
    @after { paraphrase.pop(); }
        :   '<=>'           ;
EQUAL             
    @init { paraphrase.push("=="); }
    @after { paraphrase.pop(); }
         :   '=='            ;
IDENTICAL         
    @init { paraphrase.push("==="); }
    @after { paraphrase.pop(); }
        :   '==='           ;
LNOT              
    @init { paraphrase.push("!"); }
    @after { paraphrase.pop(); }
          :   '!'             ;
BNOT              
    @init { paraphrase.push("~"); }
    @after { paraphrase.pop(); }
          :   '~'             ;
NOT_EQUAL         
    @init { paraphrase.push("!="); }
    @after { paraphrase.pop(); }
         :   '!='            ;
NOT_IDENTICAL     
    @init { paraphrase.push("!=="); }
    @after { paraphrase.pop(); }
        :   '!=='           ;
fragment  //switched from combined rule
DIV               
    @init { paraphrase.push("/"); }
    @after { paraphrase.pop(); }
          :   '/'             ;
fragment  //switched from combined rule
DIV_ASSIGN        
    @init { paraphrase.push("/="); }
    @after { paraphrase.pop(); }
         :   '/='            ;
PLUS              
    @init { paraphrase.push("+"); }
    @after { paraphrase.pop(); }
          :   '+'             ;
PLUS_ASSIGN       
    @init { paraphrase.push("+="); }
    @after { paraphrase.pop(); }
         :   '+='            ;
INC               
    @init { paraphrase.push("++"); }
    @after { paraphrase.pop(); }
         :   '++'            ;
MINUS             
    @init { paraphrase.push("-"); }
    @after { paraphrase.pop(); }
          :   '-'             ;
MINUS_ASSIGN      
    @init { paraphrase.push("-="); }
    @after { paraphrase.pop(); }
         :   '-='            ;
DEC               
    @init { paraphrase.push("--"); }
    @after { paraphrase.pop(); }
         :   '--'            ;
STAR              
    @init { paraphrase.push("*"); }
    @after { paraphrase.pop(); }
          :   '*'             ;
STAR_ASSIGN       
    @init { paraphrase.push("*="); }
    @after { paraphrase.pop(); }
         :   '*='            ;
MOD               
          :   '%'             ;
MOD_ASSIGN        
         :   '%='            ;
SR                
    @init { paraphrase.push(">>"); }
    @after { paraphrase.pop(); }
         :   '>>'            ;
SR_ASSIGN         
    @init { paraphrase.push(">>="); }
    @after { paraphrase.pop(); }
        :   '>>='           ;
BSR               
    @init { paraphrase.push(">>>"); }
    @after { paraphrase.pop(); }
        :   '>>>'           ;
BSR_ASSIGN        
    @init { paraphrase.push(">>>="); }
    @after { paraphrase.pop(); }
       :   '>>>='          ;
GE                
    @init { paraphrase.push(">="); }
    @after { paraphrase.pop(); }
         :   '>='            ;
GT                
    @init { paraphrase.push(">"); }
    @after { paraphrase.pop(); }
          :   '>'             ;
SL                
    @init { paraphrase.push("<<"); }
    @after { paraphrase.pop(); }
         :   '<<'            ;
SL_ASSIGN         
    @init { paraphrase.push("<<="); }
    @after { paraphrase.pop(); }
        :   '<<='           ;
LE                
    @init { paraphrase.push("<="); }
    @after { paraphrase.pop(); }
         :   '<='            ;
LT                
    @init { paraphrase.push("<"); }
    @after { paraphrase.pop(); }
          :   '<'             ;
BXOR              
    @init { paraphrase.push("^"); }
    @after { paraphrase.pop(); }
          :   '^'             ;
BXOR_ASSIGN       
    @init { paraphrase.push("^="); }
    @after { paraphrase.pop(); }
         :   '^='            ;
BOR               
    @init { paraphrase.push("|"); }
    @after { paraphrase.pop(); }
          :   '|'             ;
BOR_ASSIGN        
    @init { paraphrase.push("|="); }
    @after { paraphrase.pop(); }
         :   '|='            ;
LOR               
    @init { paraphrase.push("||"); }
    @after { paraphrase.pop(); }
         :   '||'            ;
BAND              
    @init { paraphrase.push("&"); }
    @after { paraphrase.pop(); }
          :   '&'             ;
BAND_ASSIGN       
    @init { paraphrase.push("&="); }
    @after { paraphrase.pop(); }
         :   '&='            ;
LAND              
    @init { paraphrase.push("&&"); }
    @after { paraphrase.pop(); }
         :   '&&'            ;
SEMI              
    @init { paraphrase.push(";"); }
    @after { paraphrase.pop(); }
          :   ';'             ;
fragment
DOLLAR            
    @init { paraphrase.push("$"); }
    @after { paraphrase.pop(); }
          :   '$'             ;
RANGE_INCLUSIVE   
    @init { paraphrase.push(".."); }
    @after { paraphrase.pop(); }
         :   '..'            ;
RANGE_EXCLUSIVE   
    @init { paraphrase.push("..<"); }
    @after { paraphrase.pop(); }
        :   '..<'           ;
TRIPLE_DOT        
    @init { paraphrase.push("..."); }
    @after { paraphrase.pop(); }
        :   '...'           ;
SPREAD_DOT        
    @init { paraphrase.push("*."); }
    @after { paraphrase.pop(); }
         :   '*.'            ;
OPTIONAL_DOT      
    @init { paraphrase.push("?."); }
    @after { paraphrase.pop(); }
         :   '?.'            ;
ELVIS_OPERATOR    
    @init { paraphrase.push("?:"); }
    @after { paraphrase.pop(); }
         :   '?:'            ;
MEMBER_POINTER    
    @init { paraphrase.push(".&"); }
    @after { paraphrase.pop(); }
         :   '.&'            ;
REGEX_FIND        
    @init { paraphrase.push("=~"); }
    @after { paraphrase.pop(); }
         :   '=~'            ;
REGEX_MATCH       
    @init { paraphrase.push("==~"); }
    @after { paraphrase.pop(); }
        :   '==~'           ;
STAR_STAR         
    @init { paraphrase.push("**"); }
    @after { paraphrase.pop(); }
         :   '**'            ;
STAR_STAR_ASSIGN  
    @init { paraphrase.push("**="); }
    @after { paraphrase.pop(); }
        :   '**='           ;
CLOSABLE_BLOCK_OP 
    @init { paraphrase.push("->"); }
    @after { paraphrase.pop(); }
         :   '->'            ;

// Whitespace -- ignored
WS
    @init { paraphrase.push("whitespace"); }
    @after { paraphrase.pop(); }

    :
        (
            options { greedy=true; }:
            ' '
        |   '\t'
        |   '\f'
        |   '\\' ONE_NL[false]
        )+
        { $channel=HIDDEN; }
    ;

fragment
ONE_NL[boolean check]
    @init { paraphrase.push("a newline"); }
    @after { paraphrase.pop(); }

 :   // handle newlines, which are significant in Groovy
        ( 
        :   '\r\n'  // Evil DOS
        |   '\r'    // Macintosh
        |   '\n'    // Unix (the right way)
        )
        {
            // update current line number for error reporting
            newlineCheck(check);
        }
    ;
    
// Group any number of newlines (with comments and whitespace) into a single token.
// This reduces the amount of parser lookahead required to parse around newlines.
// It is an invariant that the parser never sees NLS tokens back-to-back.
NLS
    @init { paraphrase.push("some newlines, whitespace or comments"); }
    @after { paraphrase.pop(); }

    :   ONE_NL[true]
        (   {!whitespaceIncluded}?
            (ONE_NL[true] | WS | SL_COMMENT | ML_COMMENT)+
            // (gobble, gobble)*
        )?
        // Inside (...) and [...] but not {...}, ignore newlines.
        {   if (whitespaceIncluded) {
                // keep the token as-is
            } else if (parenLevel != 0) {
                // when directly inside parens, all newlines are ignored here
                $channel=HIDDEN;
            } else {
                // inside {...}, newlines must be explicitly matched as 'nls!'
                //TODO: not workin setText("<newline>");
            }
        }
    ;    

// Single-line comments        
SL_COMMENT
    @init { paraphrase.push("a single line comment"); }
    @after { paraphrase.pop(); }

    :   '//'
        (
            options {  greedy = true;  }:
            // '\uffff' means the EOF character.
            // This will fix the issue GROOVY-766 (infinite loop).
            ~('\n'|'\r'|'\uffff')
        )*
        { $channel=HIDDEN;}
        //This might be significant, so don't swallow it inside the comment:
        //ONE_NL
    ;
    
// Script-header comments
SH_COMMENT
    @init { paraphrase.push("a single line comment"); }
    @after { paraphrase.pop(); }

    :   {getLine() == 1 && getColumn() == 1}? '#!'
        (
            options {  greedy = true;  }:
            // '\uffff' means the EOF character.
            // This will fix the issue GROOVY-766 (infinite loop).
            ~('\n'|'\r'|'\uffff')
        )*
        { $channel=HIDDEN;}
       
           ;    
          
ML_COMMENT //This might be significant, so don't swallow it inside the comment:
        //ONE_NL
    :   '/*' (options {greedy=false;} : .)* '*/' {$channel=HIDDEN;}
    ;

fragment    
STRING_CH
@init { paraphrase.push("a string character"); }
@after { paraphrase.pop(); }
    :   ~('"'|'\''|'\\'|'$'|'\n'|'\r'|'\uffff')
    ;
    
// string literals
STRING_LITERAL
@init { paraphrase.push("a string literal"); int tt=0; }
@after { paraphrase.pop(); }        
    :   ('\'\'\'') =>  //...shut off ambiguity warning
        '\'\'\'' {$channel=HIDDEN;}
        (   STRING_CH | ESC | '"' | '$' | STRING_NL[true]
        |   ('\'' (~'\'' | '\'' ~'\'')) => '\''  // allow 1 or 2 close quotes
        )*
        '\'\'\'' {$channel=HIDDEN;}
    |   '\''{$channel=HIDDEN;}
                                {++suppressNewline;}
        (   STRING_CH | ESC | '"' | '$'  )*
                                {--suppressNewline;}
        '\'' {$channel=HIDDEN;}
    |   ('"""') =>  //...shut off ambiguity warning
        '\"""' {$channel=HIDDEN;}
        tt=STRING_CTOR_END[true, /*tripleQuote:*/ true]
        { $type = tt; }
    |   '"' {$channel=HIDDEN;}
                                {++suppressNewline;}
        tt=STRING_CTOR_END[true, /*tripleQuote:*/ false]
        { $type = tt; }
    ;


fragment
STRING_CTOR_END[boolean fromStart, boolean tripleQuote]
@init { paraphrase.push("a string literal end"); boolean dollarOK = false; }
@after { paraphrase.pop(); }
//returns [int tt=STRING_CTOR_END]
    :
        (
            options {  greedy = true;  }:
            STRING_CH | ESC | '\'' | STRING_NL[tripleQuote]
        |   ('"' (~'"' | '"' ~'"'))=> {tripleQuote}? '"'  // allow 1 or 2 close quotes
        )*
        (   (   { !tripleQuote }? '"' {$channel=HIDDEN;}
            |   {  tripleQuote }? '"""' {$channel=HIDDEN;}
            )
            {
                if (fromStart)      tt = STRING_LITERAL;  // plain string literal!
                if (!tripleQuote)   {--suppressNewline;}
                // done with string constructor!
                //assert(stringCtorState == 0);
            }
        |   {dollarOK = atValidDollarEscape();}
            '$' {$channel=HIDDEN;}
            {
                require(dollarOK,
                    "illegal string body character after dollar sign",
                    "either escape a literal dollar sign \"\\$5\" or bracket the value expression \"${5}\"");
                // Yes, it's a string constructor, and we've got a value part.
                tt = (fromStart ? STRING_CTOR_START : STRING_CTOR_MIDDLE);
                stringCtorState = SCS_VAL + (tripleQuote? SCS_TQ_TYPE: SCS_SQ_TYPE);
            }
        )
        { $type = tt; }
    ;

fragment ESCAPED_SLASH  : '$' '/' { setText('/'); };

fragment ESCAPED_DOLLAR : '$' '$' { setText('$'); };

fragment
REGEXP_SYMBOL
@init { paraphrase.push("a multiline regular expression character"); }
@after { paraphrase.pop(); }
    :
        (
            ~('*'|'/'|'$'|'\\'|'\n'|'\r'|'\uffff')
        |   { LA(2)!='/' && LA(2)!='\n' && LA(2)!='\r' }? '\\' // backslash only escapes '/' and EOL
        |   '\\' '/'                   { setText('/'); }
        |   STRING_NL[true]
        |  '\\' ONE_NL[false]
        )
        ('*')*      // stars handled specially to avoid ambig. on /**/
    ;

fragment
DOLLAR_REGEXP_SYMBOL
@init { paraphrase.push("a multiline dollar escaping regular expression character"); }
@after { paraphrase.pop(); }
    :
        (
            ~('$' | '\\' | '/' | '\n' | '\r' | '\uffff')
        |   { LA(2)!='\n' && LA(2)!='\r' }? '\\'               // backslash only escapes EOL
        |   ('/' ~'$') => '/'                                  // allow a slash if not followed by a $
        |   STRING_NL[true]
        |  '\\' ONE_NL[false]
        )
    ;

// escape sequence -- note that this is protected; it can only be called
// from another lexer rule -- it will not ever directly return a token to
// the parser
// There are various ambiguities hushed in this rule. The optional
// '0'...'9' digit matches should be matched here rather than letting
// them go back to STRING_LITERAL to be matched. ANTLR does the
// right thing by matching immediately; hence, it's ok to shut off
// the FOLLOW ambig warnings.
fragment
ESC
    :   '\\'
        (       'n'    {setText("\n");}
        |       'r'    {setText("\r");}
        |       't'    {setText("\t");}
        |       'b'    {setText("\b");}
        |       'f'    {setText("\f");}
        |       '"'    {setText("\"");}
        |       '\''   {setText("\'");}
        |       '/'    {setText("/");}
        |       '\\'   {setText("\\");}
        |       ('u')+ {setText("");}
        
         i=HEX_DIGIT j=HEX_DIGIT k=HEX_DIGIT l=HEX_DIGIT   {setText(ParserUtil.hexToChar(i.getText(),j.getText(),k.getText(),l.getText()));}
        |   '0'..'3'
            (
            	//TODO: verify
                //options {
                //    warnWhenFollowAmbig = false;
                //}
            :   '0'..'7'
                (
                	//TODO: verify
                    //options {
                    //    warnWhenFollowAmbig = false;
                    //}
                :   '0'..'7'
                )?
            )?
            {char ch = (char)Integer.parseInt(getText,8); setText(ch);}
       |   '4'..'7'
            (
            	//TODO: verify
                //options {
                //    warnWhenFollowAmbig = false;
                //}
            :   '0'..'7'
            )?
            {char ch = (char)Integer.parseInt(getText,8); setText(ch);}
        )
       |  '\\' ONE_NL[false]
      ;
      

REGEXP_LITERAL
@init {paraphrase.push("a multiline regular expression literal"); int tt=0;}
@after { paraphrase.pop(); }
    :   {allowRegexpLiteral()}?
        '/' {$channel=HIDDEN;}
        {++suppressNewline;}
        //Do this, but require it to be non-trivial:  REGEXP_CTOR_END[true]
        // There must be at least one symbol or $ escape, lest the regexp collapse to '//'.
        // (This should be simpler, but I don't know how to do it w/o ANTLR warnings vs. '//' comments.)
        (
            REGEXP_SYMBOL
            tt=REGEXP_CTOR_END[true]
        |   {!atValidDollarEscape()}? '$'
            tt=REGEXP_CTOR_END[true]
        |   '$' {$channel=HIDDEN;}
            {
                // Yes, it's a regexp constructor, and we've got a value part.
                tt = STRING_CTOR_START;
                stringCtorState = SCS_VAL + SCS_RE_TYPE;
            }
        )
        { $type = tt; }

    |   DIV                 { $type = DIV; }
    |   DIV_ASSIGN          { $type = DIV_ASSIGN; }
    ;
    

DOLLAR_REGEXP_LITERAL
@init {paraphrase.push("a multiline dollar escaping regular expression literal"); int tt=0;}
@after { paraphrase.pop(); }
    :   {allowRegexpLiteral()}? '$/' {$channel=HIDDEN;}
        // Do this, but require it to be non-trivial:  DOLLAR_REGEXP_CTOR_END[true]
        // There must be at least one symbol or $ escape, otherwise the regexp collapses.
        (
            DOLLAR_REGEXP_SYMBOL
            tt=DOLLAR_REGEXP_CTOR_END[true]
        | {!atValidDollarEscape()}? '$'
            tt=DOLLAR_REGEXP_CTOR_END[true]
        | '$' {$channel=HIDDEN;}
            {
                // Yes, it's a regexp constructor, and we've got a value part.
                tt = STRING_CTOR_START;
                stringCtorState = SCS_VAL + SCS_DRE_TYPE;
            }
        )
        { $type = tt; }
    ;
    
fragment
REGEXP_CTOR_END[boolean fromStart]
@init {paraphrase.push("a multiline regular expression literal end"); }
@after { paraphrase.pop(); }
//returns [int tt=STRING_CTOR_END]
    :
        (
            options {  greedy = true;  }:
            REGEXP_SYMBOL
        |
            {!atValidDollarEscape()}? '$'
        )*
        (   '/' {$channel=HIDDEN;}
            {
                if (fromStart)      tt = STRING_LITERAL;  // plain regexp literal!
                {--suppressNewline;}
                // done with regexp constructor!
                //assert(stringCtorState == 0);
            }
        |   '$' {$channel=HIDDEN;}
            {
                // Yes, it's a regexp constructor, and we've got a value part.
                tt = (fromStart ? STRING_CTOR_START : STRING_CTOR_MIDDLE);
                stringCtorState = SCS_VAL + SCS_RE_TYPE;
            }
        )
        {   $type = tt;  }
    ;
    
fragment
DOLLAR_REGEXP_CTOR_END[boolean fromStart]
@init {paraphrase.push("a multiline dollar escaping regular expression literal end"); }
@after { paraphrase.pop(); }
//returns [int tt=STRING_CTOR_END]
    :
        (
            options {  greedy = true;  }:
            { !(LA(1) == '/' && LA(2) == '$') }? DOLLAR_REGEXP_SYMBOL
        |
            ('$' '/') => ESCAPED_SLASH
        |
            ('$' '$') => ESCAPED_DOLLAR
        |
            {!atValidDollarEscape() && !atDollarSlashEscape() && !atDollarDollarEscape()}? '$'
        )*
        (
            '/$' {$channel=HIDDEN;}
            {
                if (fromStart)      tt = STRING_LITERAL;  // plain regexp literal!
            }
        |   '$' {$channel=HIDDEN;}
            {
                // Yes, it's a regexp constructor, and we've got a value part.
                tt = (fromStart ? STRING_CTOR_START : STRING_CTOR_MIDDLE);
                stringCtorState = SCS_VAL + SCS_DRE_TYPE;
            }
        )
        {   $type = tt;  }
    ;        
            
fragment
STRING_NL[boolean allowNewline]
@init {paraphrase.push("a newline inside a string");}
@after { paraphrase.pop(); }
    :  {if (!allowNewline) throw new MismatchedCharException('\n', '\n', true, this); }
       ONE_NL[false] { setText("\n"); }
    ;

// hexadecimal digit (again, note it's protected!)
fragment
HEX_DIGIT
@init {paraphrase.push("a hexadecimal digit");}
@after { paraphrase.pop(); }
    :   ('0'..'9'|'A'..'F'|'a'..'f')
    ;
    
// a dummy rule to force vocabulary to be all characters (except special
// ones that ANTLR uses internally (0 to 2)
fragment
VOCAB
@init {paraphrase.push("a character");}
@after { paraphrase.pop(); }
    :   '\u007f'..'\u00FF'
    ;    
    
fragment
LETTER
@init {paraphrase.push("a letter");}
@after { paraphrase.pop(); }
    :   'a'..'z'|'A'..'Z'|'\u00C0'..'\u00D6'|'\u00D8'..'\u00F6'|'\u00F8'..'\u00FF'|'\u0100'..'\uFFFE'|'_'
    // TODO:  Recognize all the Java identifier starts here (except '$').
    ;

fragment
DIGIT
@init {paraphrase.push("a digit");}
@after { paraphrase.pop(); }
    :   '0'..'9'
    // TODO:  Recognize all the Java identifier parts here (except '$').
    ;
    
fragment
DIGITS_WITH_UNDERSCORE
@init {paraphrase.push("a sequence of digits and underscores, bordered by digits");}
@after { paraphrase.pop(); }
    :   DIGIT (DIGITS_WITH_UNDERSCORE_OPT)?
    ;

fragment
DIGITS_WITH_UNDERSCORE_OPT
@init {paraphrase.push("a sequence of digits and underscores with maybe underscore starting");}
@after { paraphrase.pop(); }
    :   (DIGIT | '_')* DIGIT
    ;

// a numeric literal    
NUM_INT
@init {paraphrase.push("a numeric literal");}
@after { paraphrase.pop(); }
    : DIGIT
    | HEX_DIGIT
    ;
    
IDENT
@init {paraphrase.push("an identifier"); }
@after { paraphrase.pop(); }
        :   {stringCtorState == 0}? (DOLLAR|LETTER) (LETTER|DIGIT|DOLLAR)*
         {
            if (stringCtorState != 0) {
                if (LA(1) == '.' && LA(2) != '$' &&
                        Character.isJavaIdentifierStart(LA(2))) {
                    // pick up another name component before going literal again:
                    restartStringCtor(false);
                } else {
                    // go back to the string
                    restartStringCtor(true);
                }
            }
            int ttype = testLiteralsTable(IDENT);
            // Java doesn't have the keywords 'as', 'in' or 'def so we make some allowances
            // for them in package names for better integration with existing Java packages
            if ((ttype == LITERAL_as || ttype == LITERAL_def || ttype == LITERAL_in) &&
                (LA(1) == '.' || lastSigTokenType == DOT || lastSigTokenType == LITERAL_package)) {
                ttype = IDENT;
            }
            if (ttype == LITERAL_static && LA(1) == '.') {
                ttype = IDENT;
            }

        /* The grammar allows a few keywords to follow dot.
         * TODO: Reinstate this logic if we change or remove keywordPropertyNames.
            if (ttype != IDENT && lastSigTokenType == DOT) {
                // A few keywords can follow a dot:
                switch (ttype) {
                case LITERAL_this: case LITERAL_super: case LITERAL_class:
                    break;
                default:
                    ttype = LITERAL_in;  // the poster child for bad dotted names
                }
            }
        */
            $type = tt;

            // check if "assert" keyword is enabled
            if (assertEnabled && "assert".equals(getText)) {
                $type = LITERAL_assert; // set token type for the rule in the parser
            }
            // check if "enum" keyword is enabled
            if (enumEnabled && "enum".equals(getText)) {
                $type = LITERAL_enum; // set token type for the rule in the parser
            }
        }
        ;

// JDK 1.5 token for annotations and their declarations
// also a groovy operator for actual field access e.g. 'telson.@age'
AT
@init {paraphrase.push("'@'");}
@after { paraphrase.pop(); }
    :   '@'
    ;
    
// a couple protected methods to assist in matching floating point numbers
fragment
EXPONENT
@init {paraphrase.push("an exponent");}
@after { paraphrase.pop(); }
    :   ('e'|'E') ('+'|'-')? ('0'..'9'|'_')* ('0'..'9')
    ;


fragment
FLOAT_SUFFIX
@init {paraphrase.push("a float or double suffix");}
@after { paraphrase.pop(); }
    :   'f'|'F'|'d'|'D'
    ;

fragment
BIG_SUFFIX
@init {paraphrase.push("a big decimal suffix");}
@after { paraphrase.pop(); }
    :   'g'|'G'
    ;
    
// Note: Please don't use physical tabs.  Logical tabs for indent are width 4.
// Here's a little hint for you, Emacs:
// Local Variables:
// tab-width: 4
// mode: antlr-mode
// indent-tabs-mode: nil
// End:
