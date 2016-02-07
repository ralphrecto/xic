package mjw297;

import java_cup.runtime.*;

%%

%cupsym Sym
%cup

%class Lexer
%unicode
%line
%column

%{
    private Symbol symbol(int type) {
        return new Symbol(type, yyline + 1, yycolumn + 1);
    }

    private Symbol symbol(int type, Object value) {
        return new Symbol(type, yyline + 1, yycolumn + 1, value);
    }
%}

/* Integer Literals */
DecIntLiteral = 0 | [1-9][0-9]*

/* Main Character Classes */
LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

/* Whitespace */
Whitespace = {LineTerminator} | [ \t\f]

/* Comments */
Comment = "//".*\n

/* Identifiers */
Identifier = [a-zA-Z][a-zA-Z_0-9\']*

%%

<YYINITIAL> {
	
	/* Keywords */
	"while"		{ return symbol(Sym.WHILE);  }
	"if"		{ return symbol(Sym.IF);     }
	"else"		{ return symbol(Sym.ELSE);   }
    "return"	{ return symbol(Sym.RETURN); }
	"int"		{ return symbol(Sym.INT);    }
	"bool"		{ return symbol(Sym.BOOL);	 }
	"use"		{ return symbol(Sym.USE);	 }
	"length"	{ return symbol(Sym.LENGTH); }
	"true"		{ return symbol(Sym.TRUE);	 }
	"false"		{ return symbol(Sym.FALSE);	 } 

	/* Symbols */
    "-"			{ return symbol(Sym.MINUS);      } 
    "!"			{ return symbol(Sym.BANG);       } 
    "*"			{ return symbol(Sym.STAR);       }
    "*>>"		{ return symbol(Sym.HIGHMULT);   }           
    "/"			{ return symbol(Sym.DIV);        }           
    "%"			{ return symbol(Sym.MOD);        }           
    "+"			{ return symbol(Sym.PLUS);       }           
    "="			{ return symbol(Sym.EQ);         }           
    "<"			{ return symbol(Sym.LT);         }           
    "<="		{ return symbol(Sym.LTE);        }           
    ">="		{ return symbol(Sym.GTE);        }           
    ">"			{ return symbol(Sym.GT);         }           
    "=="		{ return symbol(Sym.EQEQ);       }           
    "!="		{ return symbol(Sym.NEQ);        }           
    "&"			{ return symbol(Sym.AMP);        }           
    "|"			{ return symbol(Sym.BAR);        }           
    ";"			{ return symbol(Sym.SEMICOLON);  }           
    "("			{ return symbol(Sym.LPAREN);     }           
    ")"			{ return symbol(Sym.RPAREN);     }           
    "["			{ return symbol(Sym.LBRACKET);   }           
    "]"			{ return symbol(Sym.RBRACKET);   }           
    "{"			{ return symbol(Sym.LBRACE);     }           
    "}"			{ return symbol(Sym.RBRACE);     }           
    "_"			{ return symbol(Sym.UNDERSCORE); }   
    ","			{ return symbol(Sym.COMMA);      }
    ":"			{ return symbol(Sym.COLON);      }           

    /* Numeric Literals */
    {DecIntLiteral} { return symbol(Sym.NUM, new Integer(yytext());    }

    /* Comments */
    {Comment}       { /* ignore */               }

    /* Whitespace */
    {Whitespace}    { /* ignore */               }

    /* Identifiers */
    {Identifier}    { return symbol(Sym.ID, yytext());     }
}
                
                
                
[^] { throw new Error("Illegal character <"+ yytext()+">"); }
