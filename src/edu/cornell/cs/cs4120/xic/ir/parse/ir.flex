package edu.cornell.cs.cs4120.xic.ir.parse;

import java_cup.runtime.Symbol;

import java.math.BigInteger;

@SuppressWarnings({"unused", "fallthrough", "all"})
%%

%public
%class IRLexer
%function next_token
%cup

%unicode
%pack

%line
%column
%char

%{
    private Symbol number(String s) {
        BigInteger x = new BigInteger(s);
        if (x.bitLength() > 64) {
            return lexError("Number literal \"" +
                        yytext() + "\" out of range.");
        }
        return new Number(x.longValue());
    }

    private Symbol lexError(String msg) {
        System.err.println(msg);
        return new LexErrorToken();
    }

static class Name extends Symbol {
    protected String name;

    public Name(String name) {
        super(IRSym.ATOM, name);
        this.name = name;
    }
}

static class Number extends Symbol {
    public Number(long val) {
        super(IRSym.NUMBER, val);
    }
}

static class LexErrorToken extends Symbol {
    public LexErrorToken() {
        super(IRSym.error);
    }

    @Override
    public String toString() {
        return "lexical error";
    }
}
%}

%eofval{
    return new Symbol(IRSym.EOF);
%eofval}

LineTerminator = \n|\r|\r\n

WhiteSpace = [ \t\f] | {LineTerminator}

/* Identifiers */
Identifier = [:jletter:] [:jletterdigit:]*

/* Integer Literals */
DecimalNumeral = 0 | "-"?[1-9][0-9]*

%%

"COMPUNIT"          { return new Symbol(IRSym.COMPUNIT);   }
"FUNC"              { return new Symbol(IRSym.FUNC);       }
"MOVE"              { return new Symbol(IRSym.MOVE);       }
"EXP"               { return new Symbol(IRSym.EXP);        }
"SEQ"               { return new Symbol(IRSym.SEQ);        }
"JUMP"              { return new Symbol(IRSym.JUMP);       }
"CJUMP"             { return new Symbol(IRSym.CJUMP);      }
"LABEL"             { return new Symbol(IRSym.LABEL);      }
"RETURN"            { return new Symbol(IRSym.RETURN);     }
"CONST"             { return new Symbol(IRSym.CONST);      }
"TEMP"              { return new Symbol(IRSym.TEMP);       }
"MEM"               { return new Symbol(IRSym.MEM);        }
"CALL"              { return new Symbol(IRSym.CALL);       }
"NAME"              { return new Symbol(IRSym.NAME);       }
"ESEQ"              { return new Symbol(IRSym.ESEQ);       }

"ADD"               { return new Symbol(IRSym.ADD);        }
"SUB"               { return new Symbol(IRSym.SUB);        }
"MUL"               { return new Symbol(IRSym.MUL);        }
"HMUL"              { return new Symbol(IRSym.HMUL);       }
"DIV"               { return new Symbol(IRSym.DIV);        }
"MOD"               { return new Symbol(IRSym.MOD);        }
"AND"               { return new Symbol(IRSym.AND);        }
"OR"                { return new Symbol(IRSym.OR);         }
"XOR"               { return new Symbol(IRSym.XOR);        }
"LSHIFT"            { return new Symbol(IRSym.LSHIFT);     }
"RSHIFT"            { return new Symbol(IRSym.RSHIFT);     }
"ARSHIFT"           { return new Symbol(IRSym.ARSHIFT);    }
"EQ"                { return new Symbol(IRSym.EQ);         }
"NEQ"               { return new Symbol(IRSym.NEQ);        }
"LT"                { return new Symbol(IRSym.LT);         }
"GT"                { return new Symbol(IRSym.GT);         }
"LEQ"               { return new Symbol(IRSym.LEQ);        }
"GEQ"               { return new Symbol(IRSym.GEQ);        }

"("                 { return new Symbol(IRSym.LPAREN);     }
")"                 { return new Symbol(IRSym.RPAREN);     }

{Identifier}        { return new Name(yytext()); }
{DecimalNumeral}    { return number(yytext()); }

{WhiteSpace}        { /* ignore */ }
 
/* Fallthrough case: anything not matched above is an error */
[^]                 { return lexError("Illegal character \"" +
                                 yytext() + "\""); }
