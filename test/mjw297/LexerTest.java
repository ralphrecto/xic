package mjw297;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java_cup.runtime.*;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class LexerTest {
	private Symbol eof = new Symbol(Sym.EOF, -1, -1);

    /**
     * {@code lex(s)} uses the {@code Lexer} class to repeatedly lex tokens
     * from {@code s} until the EOF token is reached. The EOF token <i>is</i>
     * included in the returned list.
     */
	private List<Symbol> lex(String s) throws IOException {
		List<Symbol> result = new ArrayList<>();
		Lexer l = new Lexer(new StringReader(s));
		Symbol sym = l.next_token();

		// Collect all tokens until EOF
		while (sym.sym != Sym.EOF) {
			result.add(sym);
			sym = l.next_token();
		}

		result.add(sym);
		return result;
	}

    /**
     * {@code assertSymEquals(e, a)} asserts that the symbol codes, values,
     * line and column number of {@code e} and {@code a} are equal.
     */
	private void assertSymEquals(Symbol expected, Symbol actual) {
		assertSymEquals(Arrays.asList(expected), Arrays.asList(actual));
	}

    /**
     * {@code assertSymEquals(e1..en, a1..am)} asserts that the {@code n == m}
     * and that for all {@code i} in {@code 1..n}, {@code assertSymEquals(ei,
     * ai)}.
     */
    private void assertSymEquals(List<Symbol> expecteds, List<Symbol> actuals) {
        assertEquals("Error: number of tokens not equal.", expecteds.size(), actuals.size());
        for (int i = 0; i < expecteds.size(); ++i) {
            Symbol expected = expecteds.get(i);
            Symbol actual = actuals.get(i);
            assertEquals("Error: symbol codes not equal.", expected.sym, actual.sym);
            assertEquals("Error: symbol values not equal.", expected.value, actual.value);
			assertEquals("Error: symbol row not equal.", expected.left, actual.left);
			assertEquals("Error: symbol column not equal.", expected.right, actual.right);
        }
    }

    /**
     * {@code assertLexedStringEquals(e, s)} asserts that {@code lex(s) == [new
     * Symbol(Sym.STRING, 1, 1, e), eof]}. That is, it asserts that {@code s}
     * is lexed into exactly two tokens. The second is the EOF token. The first
     * a string token starting at row 1 and column 1 and with contents {@code
     * e}. This method makes testing string lexing much simpler.
     */
    private void assertLexedStringEquals(String expected, String s) throws IOException {
        assertSymEquals(
            Arrays.asList(new Symbol(Sym.STRING, 1, 1, expected), eof),
            lex(s));
    }

    @Test
    public void symbolEqualsTest() throws IOException {
        Symbol a = new Symbol(Sym.EQEQ);
        Symbol b = new Symbol(Sym.EQEQ);
        assertSymEquals(a, b);
    }

	@Test
	public void keywordTest() throws IOException {
		Lexer  l = new Lexer(new StringReader("while"));
		Symbol s = l.next_token();
		Symbol expected = new Symbol(Sym.WHILE, 1, 1);
        assertSymEquals(expected, s);
	}

	@Test
	public void eofTest() throws IOException {
		Lexer  l = new Lexer(new StringReader(""));
		Symbol s = l.next_token();
		Symbol expected = new Symbol(Sym.EOF, -1, -1);
        assertSymEquals(expected, s);
	}

	@Test
	public void keywordsTest() throws IOException {
		//          000000000111111111122222222223333333333444444444455
		//          123456789012345678901234567890123456789012345678901
		String s = "while if else return int bool use length true false";
		List<Symbol> expecteds = Arrays.asList(
			new Symbol(Sym.WHILE , 1, 1),
<<<<<<< HEAD
			new Symbol(Sym.IF    , 1, 7),
			new Symbol(Sym.ELSE  , 1, 10),
			new Symbol(Sym.RETURN, 1, 15),
			new Symbol(Sym.INT   , 1, 22), 
			new Symbol(Sym.BOOL  , 1, 26),
			new Symbol(Sym.USE   , 1, 31),
			new Symbol(Sym.LENGTH, 1, 35),
			new Symbol(Sym.TRUE  , 1, 42),
			new Symbol(Sym.FALSE , 1, 47),
=======
			new Symbol(Sym.IF    , 1, 6),
			new Symbol(Sym.ELSE  , 1, 8),
			new Symbol(Sym.RETURN, 1, 12),
			new Symbol(Sym.INT   , 1, 18),
			new Symbol(Sym.BOOL  , 1, 21),
			new Symbol(Sym.USE   , 1, 25),
			new Symbol(Sym.LENGTH, 1, 28),
			new Symbol(Sym.TRUE  , 1, 34),
			new Symbol(Sym.FALSE , 1, 38),
>>>>>>> 4e10336162a9334752634bb640f22319c22ba587
			eof
		);

		assertSymEquals(expecteds, lex(s));
	}

	@Test
	public void symbolsTest() throws IOException {
		//          00000000011111111112222222222333
		//          12345678901234567890123456789012
		String s = "-!**>>/%+=<<=>=>!===&|;()[]{}_,:";
		List<Symbol> expecteds = Arrays.asList(
			new Symbol(Sym.MINUS,     1, 1),
			new Symbol(Sym.BANG,      1, 2),
         	new Symbol(Sym.STAR,      1, 3),
         	new Symbol(Sym.HIGHMULT,  1, 4),
         	new Symbol(Sym.DIV,       1, 7),
         	new Symbol(Sym.MOD,       1, 8),
         	new Symbol(Sym.PLUS,      1, 9),
         	new Symbol(Sym.EQ,        1, 10),
         	new Symbol(Sym.LT,        1, 11),
         	new Symbol(Sym.LTE,       1, 12),
         	new Symbol(Sym.GTE,       1, 14),
         	new Symbol(Sym.GT,        1, 16),
         	new Symbol(Sym.NEQ,       1, 17),
         	new Symbol(Sym.EQEQ,      1, 19),
         	new Symbol(Sym.AMP,       1, 21),
         	new Symbol(Sym.BAR,       1, 22),
         	new Symbol(Sym.SEMICOLON, 1, 23),
         	new Symbol(Sym.LPAREN,    1, 24),
         	new Symbol(Sym.RPAREN,    1, 25),
         	new Symbol(Sym.LBRACKET,  1, 26),
         	new Symbol(Sym.RBRACKET,  1, 27),
         	new Symbol(Sym.LBRACE,    1, 28),
         	new Symbol(Sym.RBRACE,    1, 29),
         	new Symbol(Sym.UNDERSCORE,1, 30),
         	new Symbol(Sym.COMMA,     1, 31),
         	new Symbol(Sym.COLON,     1, 32),
			eof
		);

		assertSymEquals(expecteds, lex(s));
	}

	@Test
	public void tokenSymbolTest() throws IOException {
		List<Symbol> expecteds = Arrays.asList(
			new Symbol(Sym.PLUS , 1, 1),
			new Symbol(Sym.WHILE, 1, 2),
			eof
		);

		assertSymEquals(expecteds, lex("+while"));
	}

	@Test
	public void singleStringTest() throws IOException {
        /* simple strings with spaces */
        assertLexedStringEquals("", "\"\"");
        assertLexedStringEquals("a", "\"a\"");
        assertLexedStringEquals("ab", "\"ab\"");
        assertLexedStringEquals("abc", "\"abc\"");
        assertLexedStringEquals("a c", "\"a c\"");
        assertLexedStringEquals("  this  is  a  string  ",
                                "\"  this  is  a  string  \"");

        /* nefarious strings without escape characters */
        // no spaces
        assertLexedStringEquals("while",   "\"while\"");
        assertLexedStringEquals("if",      "\"if\"");
        assertLexedStringEquals("else",    "\"else\"");
        assertLexedStringEquals("return",  "\"return\"");
        assertLexedStringEquals("int",     "\"int\"");
        assertLexedStringEquals("bool",    "\"bool\"");
        assertLexedStringEquals("use",     "\"use\"");
        assertLexedStringEquals("length",  "\"length\"");
        assertLexedStringEquals("true",    "\"true\"");
        assertLexedStringEquals("false",   "\"false\"");
        // 1 space on left
        assertLexedStringEquals(" while",   "\" while\"");
        assertLexedStringEquals(" if",      "\" if\"");
        assertLexedStringEquals(" else",    "\" else\"");
        assertLexedStringEquals(" return",  "\" return\"");
        assertLexedStringEquals(" int",     "\" int\"");
        assertLexedStringEquals(" bool",    "\" bool\"");
        assertLexedStringEquals(" use",     "\" use\"");
        assertLexedStringEquals(" length",  "\" length\"");
        assertLexedStringEquals(" true",    "\" true\"");
        assertLexedStringEquals(" false",   "\" false\"");
        // 1 space on right
        assertLexedStringEquals("while ",   "\"while \"");
        assertLexedStringEquals("if ",      "\"if \"");
        assertLexedStringEquals("else ",    "\"else \"");
        assertLexedStringEquals("return ",  "\"return \"");
        assertLexedStringEquals("int ",     "\"int \"");
        assertLexedStringEquals("bool ",    "\"bool \"");
        assertLexedStringEquals("use ",     "\"use \"");
        assertLexedStringEquals("length ",  "\"length \"");
        assertLexedStringEquals("true ",    "\"true \"");
        assertLexedStringEquals("false ",   "\"false \"");
        // 1 space on left and right
        assertLexedStringEquals(" while ",   "\" while \"");
        assertLexedStringEquals(" if ",      "\" if \"");
        assertLexedStringEquals(" else ",    "\" else \"");
        assertLexedStringEquals(" return ",  "\" return \"");
        assertLexedStringEquals(" int ",     "\" int \"");
        assertLexedStringEquals(" bool ",    "\" bool \"");
        assertLexedStringEquals(" use ",     "\" use \"");
        assertLexedStringEquals(" length ",  "\" length \"");
        assertLexedStringEquals(" true ",    "\" true \"");
        assertLexedStringEquals(" false ",   "\" false \"");
        // lots of spaces
        assertLexedStringEquals("   while    ",   "\"   while    \"");
        assertLexedStringEquals("   if    ",      "\"   if    \"");
        assertLexedStringEquals("   else    ",    "\"   else    \"");
        assertLexedStringEquals("   return    ",  "\"   return    \"");
        assertLexedStringEquals("   int    ",     "\"   int    \"");
        assertLexedStringEquals("   bool    ",    "\"   bool    \"");
        assertLexedStringEquals("   use    ",     "\"   use    \"");
        assertLexedStringEquals("   length    ",  "\"   length    \"");
        assertLexedStringEquals("   true    ",    "\"   true    \"");
        assertLexedStringEquals("   false    ",   "\"   false    \"");

        assertLexedStringEquals("-",    "\"-\"");
        assertLexedStringEquals("!",    "\"!\"");
        assertLexedStringEquals("*",    "\"*\"");
        assertLexedStringEquals("*>>",  "\"*>>\"");
        assertLexedStringEquals("/",    "\"/\"");
        assertLexedStringEquals("%",    "\"%\"");
        assertLexedStringEquals("+",    "\"+\"");
        assertLexedStringEquals("=",    "\"=\"");
        assertLexedStringEquals("<",    "\"<\"");
        assertLexedStringEquals("<=",   "\"<=\"");
        assertLexedStringEquals(">=",   "\">=\"");
        assertLexedStringEquals(">",    "\">\"");
        assertLexedStringEquals("==",   "\"==\"");
        assertLexedStringEquals("!=",   "\"!=\"");
        assertLexedStringEquals("&",    "\"&\"");
        assertLexedStringEquals("|",    "\"|\"");
        assertLexedStringEquals(";",    "\";\"");
        assertLexedStringEquals("(",    "\"(\"");
        assertLexedStringEquals(")",    "\")\"");
        assertLexedStringEquals("[",    "\"[\"");
        assertLexedStringEquals("]",    "\"]\"");
        assertLexedStringEquals("{",    "\"{\"");
        assertLexedStringEquals("}",    "\"}\"");
        assertLexedStringEquals("_",    "\"_\"");
        assertLexedStringEquals(",",    "\",\"");
        assertLexedStringEquals(":",    "\":\"");

        assertLexedStringEquals(
              "- !  * *>> / % + = < <= >= > == != & | ; () [ ] { } _ , :",
            "\"- !  * *>> / % + = < <= >= > == != & | ; () [ ] { } _ , :\"");
        assertLexedStringEquals(
              "-!**>>/%+=<<=>=>==!=&|;()[]{}_,:",
            "\"-!**>>/%+=<<=>=>==!=&|;()[]{}_,:\"");
        assertLexedStringEquals(
              "use io main(args: int[][]) { print(12); c3p0: int = 12; }",
            "\"use io main(args: int[][]) { print(12); c3p0: int = 12; }\"");

        /* escape characters */
        // single Xi escape character
	    assertLexedStringEquals("\t", "\"\\t\"");
	    assertLexedStringEquals("\b", "\"\\b\"");
	    assertLexedStringEquals("\n", "\"\\n\"");
	    assertLexedStringEquals("\r", "\"\\r\"");
	    assertLexedStringEquals("\f", "\"\\f\"");
	    assertLexedStringEquals("\'", "\"\\\'\"");
	    assertLexedStringEquals("\"", "\"\\\"\"");
	    assertLexedStringEquals("\\", "\"\\\\\"");

        // single Java escape character
        assertLexedStringEquals("\t", "\"\t\"");
	    assertLexedStringEquals("\b", "\"\b\"");
	    assertLexedStringEquals("\f", "\"\f\"");
	    assertLexedStringEquals("\'", "\"\'\"");
	}

    @Test
    public void singleDigitTest() throws IOException {
        //          0000000001111111111
        //          1234567890123456789
        String s = "0 3 2 5 4 7 6 9 8 1";
        List<Symbol> expecteds = Arrays.asList(
            new Symbol(Sym.NUM, 1, 1, 0),
            new Symbol(Sym.NUM, 1, 3, 3),
            new Symbol(Sym.NUM, 1, 5, 2),
            new Symbol(Sym.NUM, 1, 7, 5),
            new Symbol(Sym.NUM, 1, 9, 4),
            new Symbol(Sym.NUM, 1, 11, 7),
            new Symbol(Sym.NUM, 1, 13, 6),
            new Symbol(Sym.NUM, 1, 15, 9),
            new Symbol(Sym.NUM, 1, 17, 8),
            new Symbol(Sym.NUM, 1, 19, 1),
            eof
        );

        assertSymEquals(expecteds, lex(s));
    }


    @Test
    public void commentTest() throws IOException {
        
    }

    @Test
    public void whitespaceTest() throws IOException {

    }

    @Test
    public void identifierTest() throws IOException {

    } 
}
