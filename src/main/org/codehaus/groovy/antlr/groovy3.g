//TODO: 
// Lexer
// 1. fix lexer rules for string ctor start and end as well as for regex with and without $
// 2. verify testLiteralsTable for indnt rule

//Parser
//1. move allstring literals to tokens part to use known names insetead of T__XXX new behaviour in ANTLR 3. It will also allow to split gramar file to 
//   separate files
//2. add nlsWarn parser rule problem with antlr => operator

grammar groovy3;

options
{
    k = 2;
    output = AST; 
    //TODO: uncomment
    superClass='GroovyBaseParser';
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
    // defined explicity as change in ANTLR3
    LITERAL_class = 'class';
    LITERAL_enum = 'enum';
    LITERAL_interface = 'interface';
	LITERAL_package = 'package';
	LITERAL_import = 'import';
	LITERAL_static = 'static';
	LITERAL_def = 'def';	
	LITERAL_extends ='extends';
	LITERAL_super = 'super';
	LITERAL_void = 'void';
	LITERAL_boolean = 'boolean';
	LITERAL_byte = 'byte';
	LITERAL_char = 'char';
	LITERAL_short = 'short';
	LITERAL_int = 'int';
	LITERAL_float = 'float';
	LITERAL_long = 'long';
	LITERAL_double 	= 'double';
	LITERAL_default = 'default';
	LITERAL_throws = 'throws';
	LITERAL_implements	= 'implements';
	LITERAL_this = 'this';
	LITERAL_if = 'if';
	LITERAL_else = 'else';
	LITERAL_while = 'while';
	LITERAL_switch = 'switch';
	LITERAL_for = 'for';
	LITERAL_in = 'in';
	LITERAL_return = 'return';	
	LITERAL_break = 'break';
	LITERAL_continue = 'continue';
	LITERAL_throw = 'throw';
	LITERAL_assert = 'assert';
	LITERAL_case = 'case';
	LITERAL_try = 'try';
	LITERAL_finally = 'finally';
	LITERAL_catch = 'catch';
	LITERAL_false = 'false';
	LITERAL_instanceof = 'instanceof';
	LITERAL_new = 'new';
	LITERAL_null = 'null';
	LITERAL_true = 'true';		
	LITERAL_as = 'as';
	LITERAL_private = 'private';
	LITERAL_public = 'public';
	LITERAL_protected = 'protected';
	LITERAL_transient = 'transient';
	LITERAL_native = 'native';
	LITERAL_threadsafe = 'threadsafe';
	LITERAL_synchronized = 'synchronized';
	LITERAL_volatile = 'volatile';
}

//To disable generation of the standard exception handling code in the parser
@rulecatch { } 

//Header for lexer
@lexer::header {
   package org.codehaus.groovy.antlr.parser;
   
	import org.codehaus.groovy.antlr.*;
	import java.io.InputStream;
	import java.io.Reader;
	import java.util.*;
}

//Header for parser
@parser::header {
	package org.codehaus.groovy.antlr.parser;
   
	import org.codehaus.groovy.antlr.*;
	import java.io.InputStream;
	import java.io.Reader;
}

@lexer::members {
	
	protected int lastSigTokenType = EOF;  // last returned non-whitespace token
    
    private Stack<String> paraphrase = new Stack<String>();
	
	/** flag for enabling the "assert" keyword */
    private boolean assertEnabled = true;
    /** flag for enabling the "enum" keyword */
    private boolean enumEnabled = true;
    /** flag for including whitespace tokens (for IDE preparsing) */
    private boolean whitespaceIncluded = false;
	
    /** Bumped when inside '[x]' or '(x)', reset inside '{x}'.  See ONE_NL.  */
    protected int parenLevel = 0;
    protected int suppressNewline = 0;  // be really mean to newlines inside strings
    protected static final int SCS_TYPE = 3, SCS_VAL = 4, SCS_LIT = 8, SCS_LIMIT = 16;
    protected static final int SCS_SQ_TYPE = 0, SCS_TQ_TYPE = 1, SCS_RE_TYPE = 2, SCS_DRE_TYPE = 3;
    protected int stringCtorState = 0;  // hack string and regexp constructor boundaries
    /** Push parenLevel here and reset whenever inside '{x}'. */
    protected ArrayList parenLevelStack = new ArrayList();
    
    protected void pushParenLevel() {
        parenLevelStack.add(Integer.valueOf(parenLevel*SCS_LIMIT + stringCtorState));
        parenLevel = 0;
        stringCtorState = 0;
    }

    protected void popParenLevel() {
        int npl = parenLevelStack.size();
        if (npl == 0)  return;
        int i = ((Integer) parenLevelStack.remove(--npl)).intValue();
        parenLevel      = i / SCS_LIMIT;
        //TODO: escape special char compare with orginal code
    }
    
    protected void restartStringCtor(boolean expectLiteral) {
        if (stringCtorState != 0) {
            stringCtorState = (expectLiteral? SCS_LIT: SCS_VAL) + (stringCtorState & SCS_TYPE);
        }
    }
    
    protected void newlineCheck(boolean check) throws RecognitionException {
        if (check && suppressNewline > 0) {
            require(suppressNewline == 0,
                "end of line reached within a simple string 'x' or \"x\" or /x/",
                "for multi-line literals, use triple quotes '''x''' or \"\"\"x\"\"\" or /x/ or $/x/$");
            suppressNewline = 0;  // shut down any flood of errors
        }
    }
    
    private void require(boolean z, String problem, String solution) throws MismatchedTokenException {
        // TODO: Direct to a common error handler, rather than through the parser.
        //TODO: if (!z)  parser.requireFailed(problem, solution);
    }
   
    protected boolean atValidDollarEscape() {
        // '$' (('*')? ('{' | LETTER)) =>
        int k = 1;
        int lc = input.LA(k++);
        if (lc != '$')  return false; 
        lc = input.LA(k++);
        if (lc == '*')  lc = input.LA(k++);
        return (lc == '{' || (lc != '$' //&& Character.isJavaIdentifierStart(lc)
        ));
    }
    
    protected boolean atDollarDollarEscape() {
        return input.LA(1) == '$' && input.LA(2) == '$';
    }

    protected boolean atDollarSlashEscape() {
        return input.LA(1) == '$' && input.LA(2) == '/';
    }
    
    protected boolean allowRegexpLiteral() {
        return !isExpressionEndingToken(lastSigTokenType);
    }
    
     /** Return true for an operator or punctuation which can end an expression.
     *  Return true for keywords, identifiers, and literals.
     *  Return true for tokens which can end expressions (right brackets, ++, --).
     *  Return false for EOF and all other operator and punctuation tokens.
     *  Used to suppress the recognition of /foo/ as opposed to the simple division operator '/'.
     */
    // Cf. 'constant' and 'balancedBrackets' rules in the grammar.)
    protected static boolean isExpressionEndingToken(int ttype) {
        switch (ttype) {
        case INC:               // x++ / y
        case DEC:               // x-- / y
        case RPAREN:            // (x) / y
        case RBRACK:            // f[x] / y
        case RCURLY:            // f{x} / y
        case STRING_LITERAL:    // "x" / y
        //case STRING_CTOR_END:   
        case NUM_INT:           // 0 / y
       // case NUM_FLOAT:         // 0f / y
      //  case NUM_LONG:          // 0l / y
    //    case NUM_DOUBLE:        // 0.0 / y
  //      case NUM_BIG_INT:       // 0g / y
//        case NUM_BIG_DECIMAL:   // 0.0g / y
        case IDENT:             // x / y
        // and a bunch of keywords (all of them; no sense picking and choosing):
        case LITERAL_as:
        case LITERAL_assert:
        case LITERAL_boolean:
        case LITERAL_break:
        case LITERAL_byte:
        case LITERAL_case:
        case LITERAL_catch:
        case LITERAL_char:
        case LITERAL_class:
        case LITERAL_continue:
        case LITERAL_def:
        case LITERAL_default:
        case LITERAL_double:
        case LITERAL_else:
        case LITERAL_enum:
        case LITERAL_extends:
        case LITERAL_false:
        case LITERAL_finally:
        case LITERAL_float:
        case LITERAL_for:
        case LITERAL_if:
        case LITERAL_implements:
        case LITERAL_import:
        case LITERAL_in:
        case LITERAL_instanceof:
        case LITERAL_int:
        case LITERAL_interface:
        case LITERAL_long:
        case LITERAL_native:
        case LITERAL_new:
        case LITERAL_null:
        case LITERAL_package:
        case LITERAL_private:
        case LITERAL_protected:
        case LITERAL_public:
        case LITERAL_return:
        case LITERAL_short:
        case LITERAL_static:
        case LITERAL_super:
        case LITERAL_switch:
        case LITERAL_synchronized:
        case LITERAL_this:
        case LITERAL_threadsafe:
        case LITERAL_throw:
        case LITERAL_throws:
        case LITERAL_transient:
        case LITERAL_true:
        case LITERAL_try:
        case LITERAL_void:
        case LITERAL_volatile:
        case LITERAL_while:
            return true;
        default:
            return false;
        }
    }
}

@members {	
	List warningList;
    public List getWarningList() { return warningList; }

	// Scratch variable for last 'sep' token.
    // Written by the 'sep' rule, read only by immediate callers of 'sep'.
    // (Not entirely clean, but better than a million xx=sep occurrences.)
    private int sepToken = EOF;


	    // Error handling.  This is a funnel through which parser errors go, when the parser can suggest a solution.
    public void requireFailed(String problem, String solution) throws RecognitionException {
        // TODO: Needs more work.
        Token lt = null;
        int lineNum = Token.INVALID_TOKEN.getLine(), colNum = Token.INVALID_TOKEN.getCharPositionInLine();
        try {
            lt = input.LT(1);
            if(lt != null) {
                lineNum = lt.getLine();
                colNum = lt.getCharPositionInLine();
            }
        }
        catch (Exception ee) {
            //if(ee instanceof TokenStreamRecognitionException) {
            //    lineNum = ((TokenStreamRecognitionException) ee).recog.getLine();
            //    colNum = ((TokenStreamRecognitionException) ee).recog.getColumn();
            //}
        }
        //TODO: reason problem + ";\n   solution: " + solution
        throw new RecognitionException(input);
    }
    
    public void addWarning(String warning, String solution) {
        Token lt = null;
		lt = input.LT(1);
		// TODO: ignore exception 
        if (lt == null)  lt = Token.INVALID_TOKEN;

        Map row = new HashMap();
        row.put("warning",  warning);
        row.put("solution", solution);
        row.put("filename", input.getSourceName());
        row.put("line",     Integer.valueOf(lt.getLine()));
        row.put("column",   Integer.valueOf(lt.getCharPositionInLine()));
        // System.out.println(row);
        warningList.add(row);
    }
}

/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/

    // modifiers for Java classes, interfaces, class/instance vars and methods
modifier
    :   LITERAL_private
    |   LITERAL_public
    |   LITERAL_protected
    |   LITERAL_static
    |   LITERAL_transient
    |   FINAL
    |   ABSTRACT
    |   LITERAL_native
    |   LITERAL_threadsafe
    |   LITERAL_synchronized
    |   LITERAL_volatile
    |   STRICTFP
    ;   
  
// The primitive types.
builtInType
    :   LITERAL_void
    |   LITERAL_boolean
    |   LITERAL_byte
    |   LITERAL_char
    |   LITERAL_short
    |   LITERAL_int
    |   LITERAL_float
    |   LITERAL_long
    |   LITERAL_double
    ;    
        /*
 * Allowed keywords after dot (as a member name) and before colon (as a label).
 * Includes all Java keywords plus "in" and "as".
 */
keywordPropertyNames
    :   LITERAL_as
        | LITERAL_assert
        | LITERAL_break
        | LITERAL_case
        | LITERAL_catch
        | LITERAL_class
        | UNUSED_CONST
        | LITERAL_continue
        | LITERAL_def
        | LITERAL_default
        | UNUSED_DO
        | LITERAL_else
        | LITERAL_enum
        | LITERAL_extends
        | LITERAL_false
        | LITERAL_finally
        | LITERAL_for
        | UNUSED_GOTO
        | LITERAL_if
        | LITERAL_implements
        | LITERAL_import
        | LITERAL_in
        | LITERAL_instanceof
        | LITERAL_interface
        | LITERAL_new
        | LITERAL_null
        | LITERAL_package
        | LITERAL_return
        | LITERAL_super
        | LITERAL_switch
        | LITERAL_this
        | LITERAL_throw
        | LITERAL_throws
        | LITERAL_true
        | LITERAL_try
        | LITERAL_while
        | modifier
        | builtInType
    ;

variableName
    :   IDENT
    ;

// A (possibly-qualified) java identifier. We start with the first IDENT
// and expand its name by adding dots and following IDENTS
identifier 
	@init{Token first = input.LT(1);}
    :   i1=IDENT!
        (   options { greedy = true; } :
            d=DOT! nls! i2=IDENT!
            {i1 = #(create(DOT,".",first,input.LT(1)),i1,i2);}
        )*
        {#identifier = #i1;}
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
    
// The primitive types.
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

    :   {getLine() == 1 && getCharPositionInLine() == 1}? '#!'
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
@init { paraphrase.push("a string literal"); int type=0; }
@after { paraphrase.pop(); }        
    :    '\'\'\''
        (   STRING_CH | ESC | '"' | '$' | STRING_NL[true]
        |   ('\'' (~'\'' | '\'' ~'\'')) => '\''  // allow 1 or 2 close quotes
        )*
        '\'\'\'' {$channel=HIDDEN;}
    |   '\''
                                {++suppressNewline;}
        (   STRING_CH | ESC | '"' | '$'  )*
                                {--suppressNewline;}
        '\'' {$channel=HIDDEN;}
    |   '"'
                                {++suppressNewline;}
        (   STRING_CH | ESC | '\'' | '$'  )*
                                {--suppressNewline;}
        '"' {$channel=HIDDEN;}
    ;

fragment ESCAPED_SLASH  : '$' '/' { setText(Character.toString('/')); };

fragment ESCAPED_DOLLAR : '$' '$' { setText(Character.toString('$')); };

fragment
REGEXP_SYMBOL
@init { paraphrase.push("a multiline regular expression character"); }
@after { paraphrase.pop(); }
    :
        (
            ~('*'|'/'|'$'|'\\'|'\n'|'\r'|'\uffff')
        |   { input.LA(2)!='/' && input.LA(2)!='\n' && input.LA(2)!='\r' }? '\\' // backslash only escapes '/' and EOL
        |   '\\' '/'                   { setText(Character.toString('/')); }
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
        |   { input.LA(2)!='\n' && input.LA(2)!='\r' }? '\\'               // backslash only escapes EOL
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
        
         i=HEX_DIGIT j=HEX_DIGIT k=HEX_DIGIT l=HEX_DIGIT   
         { 
         	//TODO:find namespace for ParserUtil setText(ParserUtil.hexToChar(i.getText(),j.getText(),k.getText(),l.getText()));
         }
        |   '0'..'3'
            (
            :   '0'..'7'
                (
                :   '0'..'7'
                )?
            )?
            {char ch = (char)Integer.parseInt(getText(),8); setText(Character.toString(ch));}
       |   '4'..'7'
            (
            :   '0'..'7'
            )?
            {char ch = (char)Integer.parseInt(getText(),8); setText(Character.toString(ch));}
        )
       |  '\\' ONE_NL[false]
      ;
      

REGEXP_LITERAL
@init {paraphrase.push("a multiline regular expression literal"); int type=0;}
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
        )
        { $type = type; }

    |   DIV                 { $type = DIV; }
    |   DIV_ASSIGN          { $type = DIV_ASSIGN; }
    ;
    

DOLLAR_REGEXP_LITERAL
@init {paraphrase.push("a multiline dollar escaping regular expression literal");}
@after { paraphrase.pop(); }
    :   {allowRegexpLiteral()}? '$/' {$channel=HIDDEN;}
        // Do this, but require it to be non-trivial:  DOLLAR_REGEXP_CTOR_END[true]
        // There must be at least one symbol or $ escape, otherwise the regexp collapses.
        (
            DOLLAR_REGEXP_SYMBOL
            type=DOLLAR_REGEXP_CTOR_END[true]
        | {!atValidDollarEscape()}? '$'
            type=DOLLAR_REGEXP_CTOR_END[true]
        | '$' {$channel=HIDDEN;}
        )
        { $type = type; }
    ;
    
fragment
REGEXP_CTOR_END[boolean fromStart]
@init {paraphrase.push("a multiline regular expression literal end"); int type; }
@after { paraphrase.pop(); }
//returns [int type=STRING_CTOR_END]
    :
        (
            options {  greedy = true;  }:
            REGEXP_SYMBOL
        |
            {!atValidDollarEscape()}? '$'
        )*
        (   '/'
            {
                if (fromStart)      type = STRING_LITERAL;  // plain regexp literal!
                {--suppressNewline;}
                // done with regexp constructor!
                //assert(stringCtorState == 0);
            }
        |   '$' 
        )
    ;
    
fragment
DOLLAR_REGEXP_CTOR_END[boolean fromStart]
@init {paraphrase.push("a multiline dollar escaping regular expression literal end"); int type; }
@after { paraphrase.pop(); }
//returns [int type=STRING_CTOR_END]
    :
        (
            options {  greedy = true;  }:
            { !(input.LA(1) == '/' && input.LA(2) == '$') }? DOLLAR_REGEXP_SYMBOL
        |
            ('$' '/') => ESCAPED_SLASH
        |
            ('$' '$') => ESCAPED_DOLLAR
        |
            {!atValidDollarEscape() && !atDollarSlashEscape() && !atDollarDollarEscape()}? '$'
        )*
        (
            '/$'
            {
                if (fromStart)      type = STRING_LITERAL;  // plain regexp literal!
            }
        |   '$' 
        )
    ;        
            
fragment
STRING_NL[boolean allowNewline]
@init {paraphrase.push("a newline inside a string");}
@after { paraphrase.pop(); }
    :  {if (!allowNewline) throw new MismatchedRangeException('\n', '\n', input); }
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
                if (input.LA(1) == '.' && input.LA(2) != '$' &&
                        Character.isJavaIdentifierStart(input.LA(2))) {
                    // pick up another name component before going literal again:
                    restartStringCtor(false);
                } else {
                    // go back to the string
                    restartStringCtor(true);
                }
            }
            
            //TODO: verify testLiteralsTable
            int ttype = IDENT;
            // Java doesn't have the keywords 'as', 'in' or 'def so we make some allowances
            // for them in package names for better integration with existing Java packages
            if ((ttype == LITERAL_as || ttype == LITERAL_def || ttype == LITERAL_in) &&
                (input.LA(1) == '.' || lastSigTokenType == DOT || lastSigTokenType == LITERAL_package)) {
                ttype = IDENT;
            }
            if (ttype == LITERAL_static && input.LA(1) == '.') {
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
            $type = ttype;

            // check if "assert" keyword is enabled
            if (assertEnabled && "assert".equals(getText())) {
                $type = LITERAL_assert; // set token type for the rule in the parser
            }
            // check if "enum" keyword is enabled
            if (enumEnabled && "enum".equals(getText())) {
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
