package org.codehaus.groovy.antlr;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.Token;
import org.antlr.runtime.TokenStream;
import org.antlr.runtime.debug.*;

public class GroovyBaseParser extends DebugParser {

	public GroovyBaseParser(TokenStream input, DebugEventListener dbg) {
		super(input, dbg);
		// TODO Auto-generated constructor stub
	}

	
	List warningList;
    public List getWarningList() { return warningList; }

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
