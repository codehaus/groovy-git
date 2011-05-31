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

@members {
	private Stack<String> paraphrase = new Stack<String>();
	
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
            setType(ttype);

            // check if "assert" keyword is enabled
            if (assertEnabled && "assert".equals(getText)) {
                setType(LITERAL_assert); // set token type for the rule in the parser
            }
            // check if "enum" keyword is enabled
            if (enumEnabled && "enum".equals(getText)) {
                setType(LITERAL_enum); // set token type for the rule in the parser
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
