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
    StringBuffer sb = new StringBuffer();
	int stringStart = 0;

	/* chop takes the substring from the ith character to the
	   jth last character */
	private String chop(int i, int j) {
		return yytext().substring(i, yylength()-j);
	}

	private String chop(int j) {
		return chop(0, j);
	}

	private String chop() {
		return chop(0, 1);
	}

    private Symbol symbol(int type) {
        return new Symbol(type, yyline + 1, yycolumn + 1);
    }

    private Symbol symbol(int type, Object value) {
        return new Symbol(type, yyline + 1, yycolumn + 1, value);
    }
%}

HexEscape = \\ u [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]
  
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
	\"			{ sb.setLength(0); stringStart = yycolumn + 1; 
				  yybegin(STRING); }         

	/* Character */
	\'			{ sb.setLength(0); yybegin(CHARACTER); }

}               
               
<STRING> {
	/* End of string */
	\"			 { yybegin(YYINITIAL);
				   return symbol(Sym.STRING,
				   sb.toString());}

	/* escape characters */	
	\\t			 { sb.append('\t');		  }
	\\n			 { sb.append('\n');		  }
	\\r			 { sb.append('\r');		  }
	\\\"		 { sb.append('\"');		  }
	\\			 { sb.append('\\');		  }

	{HexEscape}	 { try {
					 int x = Integer.parseInt(chop(4,0), 16);
					 sb.append(chop(2,0));
				   } catch (NumberFormatException e) {
				   	   /* TODO: error handling */	
				   }
				 } 

	/* other unhandled escape characters */
	\\.			 { sb.append("hi"); /* TODO: error handling */ }

	/* unclosed string */
	/* TODO: need to put regex here and error handling */
	
	/* anything else */
	[^\n\r\"\\]+ { sb.append( yytext() ); }

} 

<CHARACTER> {
	/* end of character */
	\'			{ yybegin(YYINITIAL);
				  return symbol(Sym.CHAR,
				  sb.toString());}
}

                
[^] { throw new Error("Illegal character <"+ yytext()+">"); }
