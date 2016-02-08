package mjw297;

import java_cup.runtime.*;
import mjw297.XicException.*;

%%

%cupsym Sym
%cup

%class Lexer
%unicode
%line
%column

%yylexthrow XicException

%state STRING CHARACTER

%{
    // strings are lexed into sb
    StringBuffer sb = new StringBuffer();
    int stringStart = 0;

	/* chop returns the substring from the ith character to the
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

    /**
     * {@code longLiteral(s)} parses {@code s} into a Xi number literal. It is
     * a precondition that {@code s} must only contain digits; it may not
     * include letters or even a leading + or -. This function returns one of
     * three things depending on the value of {@code s}.
     *
     *   1. If {@code 0 <= s <= 9223372036854775807}, then a {@code NUM} symbol
     *      is returned
     *   2. If {@code s == "9223372036854775808"}, then a {@code BIG_NUM}
     *      symbol is returned
     *   3. If {@code s} is outside the range of a valid integer, then a
     *      IntegerLiteralOutOfBoundsException is raised.
     */
    private Symbol longLiteral(String s) throws XicException {
        try {
            if (s.equals("9223372036854775808")) {
                return symbol(Sym.BIG_NUM);
            } else {
                return symbol(Sym.NUM, new Long(s));
            }
        } catch (NumberFormatException e) {
            throw new IntegerLiteralOutOfBoundsException(row(), column(), s);
        }
    }
%}

/* Hex Escape */
HexEscape = \\ x [0-9a-fA-F] [0-9a-fA-F]
  
/* Unicode Escape */
UnicodeEscape = \\ u [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] 

/* Integer Literals */
DecIntLiteral = 0 | [1-9][0-9]*

/* Main Character Classes */
LineTerminator = \r|\n|\r\n

/* Whitespace */
Whitespace = {LineTerminator} | [ \t\f]

/* Comments */
Comment = "//"[^\r\n]*({LineTerminator}|\Z)

/* Identifiers */
Identifier = [a-zA-Z][a-zA-Z_0-9\']*

%%

<YYINITIAL> {

    /* Keywords */
    "while"     { return symbol(Sym.WHILE);  }
    "if"        { return symbol(Sym.IF);     }
    "else"      { return symbol(Sym.ELSE);   }
    "return"    { return symbol(Sym.RETURN); }
    "int"       { return symbol(Sym.INT);    }
    "bool"      { return symbol(Sym.BOOL);   }
    "use"       { return symbol(Sym.USE);    }
    "length"    { return symbol(Sym.LENGTH); }
    "true"      { return symbol(Sym.TRUE);   }
    "false"     { return symbol(Sym.FALSE);  }

    /* Symbols */
    "-"         { return symbol(Sym.MINUS);      }
    "!"         { return symbol(Sym.BANG);       }
    "*"         { return symbol(Sym.STAR);       }
    "*>>"       { return symbol(Sym.HIGHMULT);   }
    "/"         { return symbol(Sym.DIV);        }
    "%"         { return symbol(Sym.MOD);        }
    "+"         { return symbol(Sym.PLUS);       }
    "="         { return symbol(Sym.EQ);         }
    "<"         { return symbol(Sym.LT);         }
    "<="        { return symbol(Sym.LTE);        }
    ">="        { return symbol(Sym.GTE);        }
    ">"         { return symbol(Sym.GT);         }
    "=="        { return symbol(Sym.EQEQ);       }
    "!="        { return symbol(Sym.NEQ);        }
    "&"         { return symbol(Sym.AMP);        }
    "|"         { return symbol(Sym.BAR);        }
    ";"         { return symbol(Sym.SEMICOLON);  }
    "("         { return symbol(Sym.LPAREN);     }
    ")"         { return symbol(Sym.RPAREN);     }
    "["         { return symbol(Sym.LBRACKET);   }
    "]"         { return symbol(Sym.RBRACKET);   }
    "{"         { return symbol(Sym.LBRACE);     }
    "}"         { return symbol(Sym.RBRACE);     }
    "_"         { return symbol(Sym.UNDERSCORE); }
    ","         { return symbol(Sym.COMMA);      }
    ":"         { return symbol(Sym.COLON);      }

    /* String */
    \"          { sb.setLength(0);
                  startRow = row();
                  startColumn = column();
                  yybegin(STRING); }

	/* Character */
	\'			{ sb.setLength(0); 
				  startRow = row();
				  startColumn = column();
				  yybegin(CHARACTER); }

    /* Numeric Literals */
    {DecIntLiteral} { return longLiteral(yytext());    }

    /* Comments */
    {Comment}       { /* ignore */               }

    /* Whitespace */
    {Whitespace}    { /* ignore */               }

    /* Identifiers */
    {Identifier}    { return symbol(Sym.ID, yytext());     }
}

<STRING> {
    /* End of string */
    \"           { yybegin(YYINITIAL);
                   int r = startRow;
                   int c = startColumn;
                   startRow = -1;
                   startColumn = -1;
				   return new Symbol(Sym.STRING, r, c, sb.toString()); }

	/* escape characters */
	\\t			 { sb.append('\t');		  }
	\\b			 { sb.append('\b');		  }
	\\n			 { sb.append('\n');		  }
	\\r			 { sb.append('\r');		  }
	\\f			 { sb.append('\f');		  }
	\\\'		 { sb.append('\'');		  }
	\\\"		 { sb.append('\"');		  }
	\\\\		 { sb.append('\\');		  }

	{HexEscape}	 { try {
					 int x = Integer.parseInt(chop(2,0), 16);
					 sb.append((char) x);
				   } catch (NumberFormatException e) {
				   	   /* TODO: error handling */	
				   }
				 } 

	{UnicodeEscape} { try {
						int x = Integer.parseInt(chop(2,0), 16);
						sb.append((char) x);
					  } catch (NumberFormatException e) {
					  	/* TODO: error handling */
					  }
					} 	

	/* other unhandled escape characters */
	\\.			 { sb.append("hi"); /* TODO: error handling */ }

	/* unclosed string */
	{LineTerminator} {yybegin(YYINITIAL); /* TODO: error handling */} 	
	
	/* anything else */
	[^\n\r\"\\]+ { sb.append( yytext() ); }

} 

<CHARACTER> {
	/* end of character */
	\'			{ yybegin(YYINITIAL);
				  String str = sb.toString();
				  if (str.length() == 1) {
				  	char x = str.charAt(0);
					int r = startRow;
					int c = startColumn;
					startRow = -1;
					startColumn = -1;
					return new Symbol(Sym.CHAR, r, c, x);
				  } else {
				  	/* TODO: error handling */
				  } 
				}

	/* escape characters */
	\\t			 { sb.append('\t');		  }
	\\b			 { sb.append('\b');		  }
	\\n			 { sb.append('\n');		  }
	\\r			 { sb.append('\r');		  }
	\\f			 { sb.append('\f');		  }
	//\\\'		 { sb.append('\'');		  }
	\\\"		 { sb.append('\"');		  }
	\\\\		 { sb.append('\\');		  }

	{HexEscape}	 { try {
					 int x = Integer.parseInt(chop(2,0), 16);
					 sb.append((char) x);
				   } catch (NumberFormatException e) {
				   	   /* TODO: error handling */	
				   }
				 } 

	{UnicodeEscape} { try {
						int x = Integer.parseInt(chop(2,0), 16);
						sb.append((char) x);
					  } catch (NumberFormatException e) {
					  	/* TODO: error handling */
					  }
					} 	

	/* other unhandled escape characters */
	\\.			 { /* TODO: error handling */ }

	/* unclosed string */
	{LineTerminator} {yybegin(YYINITIAL); /* TODO: error handling */} 	
	
	/* anything else */
	[^\n\r\'\\]+ { sb.append( yytext() ); }
}


[^] { throw new Error("Illegal character <"+ yytext()+">"); }













