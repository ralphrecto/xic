package mjw297;

import java_cup.runtime.*;

%%

%cupsym Sym
%cup

%class Lexer
%unicode
%line
%column

%state STRING CHARACTER

%{
    // strings are lexed into sb
    StringBuffer sb = new StringBuffer();
    // If not -1, the row and column of the start of a char or string
    int startRow = -1;
    int startColumn = -1;

    private int row() {
        return yyline + 1;
    }

    private int column() {
        return yycolumn + 1;
    }

    private Symbol symbol(int type) {
        return new Symbol(type, row(), column());
    }

    private Symbol symbol(int type, Object value) {
        return new Symbol(type, row(), column(), value);
    }
%}

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

	/* String */
	\"			{ sb.setLength(0);
                  startRow = row();
                  startColumn = column();
                  yybegin(STRING); }

	/* Character */
	\'			{ sb.setLength(0); yybegin(CHARACTER); }

	/* whitespace */
}

<STRING> {
	/* End of string */
	\"			 { yybegin(YYINITIAL);
                   int r = startRow;
                   int c = startColumn;
                   startRow = -1;
                   startColumn = -1;
				   return new Symbol(Sym.STRING, r, c, sb.toString()); }
	[^\n\r\"\\]+ { sb.append(yytext()); }

	/* escape characters */
	\\t			 { sb.append('\t');		  }
	\\b			 { sb.append('\b');		  }
	\\n			 { sb.append('\n');		  }
	\\r			 { sb.append('\r');		  }
	\\f			 { sb.append('\f');		  }
	\\\'		 { sb.append('\'');		  }
	\\\"		 { sb.append('\"');		  }
	\\\\		 { sb.append('\\');		  }

}

<CHARACTER> {
	/* end of character */
	\'			{ yybegin(YYINITIAL);
				  return symbol(Sym.CHAR,
				  sb.toString());}
}


[^] { throw new Error("Illegal character <"+ yytext()+">"); }
