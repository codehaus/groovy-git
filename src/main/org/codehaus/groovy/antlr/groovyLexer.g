lexer  grammar groovyLexer;

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

@lexer::members {
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
