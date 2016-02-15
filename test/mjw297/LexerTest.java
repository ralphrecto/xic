package mjw297;

import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java_cup.runtime.*;
import mjw297.XicException.*;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class LexerTest {

    private Symbol eof = new Symbol(Sym.EOF, -1, -1);

    private Actions.Lexed lex(String s) throws IOException {
        return Actions.lex(new StringReader(s));
    }

    /**
     * {@code assertSymEquals(e, a)} is shorthand for {@code
     * assertSymEquals(Actions.Lexed([e], None), Actions.Lexed([a], None))}
     */
    private void assertSymEquals(Symbol expected, Symbol actual) {
        assertSymEquals(new Actions.Lexed(Arrays.asList(expected), Optional.empty()),
                        new Actions.Lexed(Arrays.asList(actual), Optional.empty()));
    }

    /**
     * {@code assertSymEquals(e, a)} is shorthand for {@code
     * assertSymEquals(Actions.Lexed(e, None), Actions.Lexed(a, None))}
     */
    private void assertSymEquals(List<Symbol> expecteds, Actions.Lexed actual) {
        assertSymEquals(new Actions.Lexed(expecteds, Optional.empty()), actual);
    }

    /**
     * {@code assertSymEquals(e, a)} is shorthand for {@code
     * assertSymEquals(Actions.Lexed([], e), Actions.Lexed(a, None))}
     */
    private void assertSymEquals(XicException expected, Actions.Lexed actual) {
        assertSymEquals(new Actions.Lexed(Arrays.asList(), Optional.of(expected)), actual);
    }

    /**
     * {@code assertSymEquals(Actions.Lexed(e1..en, ee), Actions.Lexed(a1..am, ae)} asserts
     * that the {@code n == m}, {@code ee == ae}, and that for all {@code i} in
     * {@code 1..n}, {@code assertSymEquals(ei, ai)}.
     */
    private void assertSymEquals(Actions.Lexed expected, Actions.Lexed actual) {
        assertEquals("Error: number of tokens not equal.",
                expected.symbols.size(), actual.symbols.size());
        for (int i = 0; i < expected.symbols.size(); ++i) {
            Symbol e = expected.symbols.get(i);
            Symbol a = actual.symbols.get(i);
            assertEquals("Error: symbol codes not equal.", e.sym, a.sym);
            assertEquals("Error: symbol values not equal.", e.value, a.value);
            assertEquals("Error: symbol row not equal.", e.left, a.left);
            assertEquals("Error: symbol column not equal.", e.right, a.right);
        }
        assertEquals("Error: exceptions not both present or absent.",
                expected.exception.isPresent(), actual.exception.isPresent());
        expected.exception.ifPresent((e) -> {
            actual.exception.ifPresent((a) -> {
                assertEquals("Error: error code not equal.", e.code, a.code);
                assertEquals("Error: exception row not equal.", e.row, a.row);
                assertEquals("Error: exception column not equal.", e.column, a.column);
            });
        });
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

    /**
     * {@code assertLexedStringEquals(e, s)} is shorthand for {@code
     * assertSymEquals(Lexed([], e), Lexed(a, None))}
     */
    private void assertLexedStringEquals(XicException e, String s) throws IOException {
        assertSymEquals(
                new Actions.Lexed(Arrays.asList(), Optional.of(e)),
                lex(s));
    }

    @Test
    public void symbolEqualsTest() throws IOException {
        Symbol a = new Symbol(Sym.EQEQ);
        Symbol b = new Symbol(Sym.EQEQ);
        assertSymEquals(a, b);
    }

    @Test
    public void keywordTest() throws IOException, XicException {
        Lexer  l = new Lexer(new StringReader("while"));
        Symbol s = l.next_token();
        Symbol expected = new Symbol(Sym.WHILE, 1, 1);
        assertSymEquals(expected, s);
    }

    @Test
    public void eofTest() throws IOException, XicException {
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
            new Symbol(Sym.IF    , 1, 7),
            new Symbol(Sym.ELSE  , 1, 10),
            new Symbol(Sym.RETURN, 1, 15),
            new Symbol(Sym.INT   , 1, 22),
            new Symbol(Sym.BOOL  , 1, 26),
            new Symbol(Sym.USE   , 1, 31),
            new Symbol(Sym.LENGTH, 1, 35),
            new Symbol(Sym.TRUE  , 1, 42),
            new Symbol(Sym.FALSE , 1, 47),
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
    public void stringTest() throws IOException, XicException {
        //Lexer  l = new Lexer(new StringReader("\"hello\t\""));
        //Symbol s = l.next_token();
        //Symbol expected = new Symbol(Sym.STRING, 1, 1, "hello\t");

        // LEXER INPUTS
        // Correct: single string
        String s1 = "\"hello\t\"";
        // Correct: empty string
        String s2 = "\"\"";
        // Correct: many strings
        String s3 = "\" hi, I really dislike form feeds >:(\"";
        // Correct: crazy unicode
        String s4 = "\"\u1008  \\u2028\ufeed\"";
        // Invalid: invalid escaped string
        String s5 = "\"\\a\"";
        // Invalid: unclosed character
        String s6 = "\"I'm unclosed!";
        // Correct: single quote inside a string
        String s7 = "\"I'm won't can't\"";

        // Expected symbols and exceptions
        String e1 = "hello\t";
        String e2 = "";
        String e3 = " hi, I really dislike form feeds >:(";
        String e4 = "\u1008  \u2028\ufeed";
        XicException e5 = new InvalidEscapeException(1, 1, "\\a");
        XicException e6 = new UnclosedStringLiteralException(1, 1, "\"I'm unclosed!");
        String e7 = "I'm won't can't";

        assertLexedStringEquals(e1, s1);
        assertLexedStringEquals(e2, s2);
        assertLexedStringEquals(e3, s3);
        assertLexedStringEquals(e4, s4);
        assertLexedStringEquals(e5, s5);
        assertLexedStringEquals(e6, s6);
        assertLexedStringEquals(e7, s7);
    }

    @Test
    public void stringHexTest() throws IOException, XicException {
        Lexer  l = new Lexer(new StringReader("\"\\x23\""));
        Symbol s = l.next_token();
        Symbol expected = new Symbol(Sym.STRING, 1, 1, "#");

        assertSymEquals(expected, s);
	}

	@Test
	public void stringUnicodeTest() throws IOException, XicException {
		Lexer l = new Lexer(new StringReader("\"\\u2013\""));
		Symbol s = l.next_token();
		Symbol expected = new Symbol(Sym.STRING, 1, 1, "–");
		
		assertSymEquals(expected, s);
	}	

	@Test
	public void stringUnicodesTest() throws IOException, XicException {
		Lexer l = new Lexer(new StringReader("\"\\u0048\\u0065\\u006C\\u006C\\u006F World\""));
		Symbol s = l.next_token();
		Symbol expected = new Symbol(Sym.STRING, 1, 1, "Hello World");

		assertSymEquals(expected, s);
	}

	@Test
	public void charTest() throws IOException, XicException {
        // LEXER INPUTS
        // Correct: single character
        String s1 = "'a'";
        // Invalid: empty character
        String s2 = "''";
        // Invalid: too many characters
        String s3 = "'char2long4me'";
        // Correct: form feed
        String s4 = "'\\f'";
        // Invalid: escaped character
        String s5 = "'\\a'";
        // Invalid: unclosed character 
        String s6 = "'\\t";
        // Correct: double quote
        String s7 = "'\"'";
        // Invalid: valid chars followed by invalid chars
        //           0000000001111111111222222
        //           1234567890123456789012345
        String s8 = "'h''i'' ''u''g''h''asdf'";

        // Expected symbols and exceptions
		List<Symbol> expected1 = Arrays.asList(
            new Symbol(Sym.CHAR, 1, 1, 'a'),
            eof
        );
        XicException expected2 = new EmptyCharacterLiteralException(1, 1); 
        XicException expected3 = new InvalidCharacterConstantException(1, 1);
        List<Symbol> expected4 = Arrays.asList(
            new Symbol(Sym.CHAR, 1, 1, '\f'),
            eof
        );
        XicException expected5 = new InvalidEscapeException(1, 1, s5);
        XicException expected6 = new UnclosedCharacterLiteralException(1, 1, s6);
        List<Symbol> expected7 = Arrays.asList(
            new Symbol(Sym.CHAR, 1, 1, '"'),
            eof
        );
        Actions.Lexed expected8 = new Actions.Lexed(Arrays.asList(
                new Symbol(Sym.CHAR, 1, 1, 'h'),
                new Symbol(Sym.CHAR, 1, 4, 'i'),
                new Symbol(Sym.CHAR, 1, 7, ' '),
                new Symbol(Sym.CHAR, 1, 10, 'u'),
                new Symbol(Sym.CHAR, 1, 13, 'g'),
                new Symbol(Sym.CHAR, 1, 16, 'h')
            ),
            Optional.of(new InvalidCharacterConstantException(1, 19))
        );

		assertSymEquals(expected1, lex(s1)); 
        assertSymEquals(expected2, lex(s2));
        assertSymEquals(expected3, lex(s3));
        assertSymEquals(expected4, lex(s4));
        assertSymEquals(expected5, lex(s5));
        assertSymEquals(expected6, lex(s6));
        assertSymEquals(expected7, lex(s7));
        assertSymEquals(expected8, lex(s8));
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
    public void numbersTest() throws IOException {
        //           00000000011111111112222222222
        //           12345678901234567890123456789
        String s1 = "0 3 2 5 4 7 6 9 8 1";
        List<Symbol> expecteds1 = Arrays.asList(
            new Symbol(Sym.NUM, 1, 1, 0l),
            new Symbol(Sym.NUM, 1, 3, 3l),
            new Symbol(Sym.NUM, 1, 5, 2l),
            new Symbol(Sym.NUM, 1, 7, 5l),
            new Symbol(Sym.NUM, 1, 9, 4l),
            new Symbol(Sym.NUM, 1, 11, 7l),
            new Symbol(Sym.NUM, 1, 13, 6l),
            new Symbol(Sym.NUM, 1, 15, 9l),
            new Symbol(Sym.NUM, 1, 17, 8l),
            new Symbol(Sym.NUM, 1, 19, 1l),
            eof
        );

        String s2 = " 100000000 17 8932 ";
        List<Symbol> expecteds2 = Arrays.asList(
            new Symbol(Sym.NUM, 1, 2, 100000000l),
            new Symbol(Sym.NUM, 1, 12, 17l),
            new Symbol(Sym.NUM, 1, 15, 8932l),
            eof
        );

        String s3 = "-9223372036854775808 9223372036854775807";
        List<Symbol> expecteds3 = Arrays.asList(
            new Symbol(Sym.MINUS, 1, 1),
            new Symbol(Sym.BIG_NUM, 1, 2),
            new Symbol(Sym.NUM, 1, 22, 9223372036854775807l),
            eof
        );

        String s4 = "9223372036854775809";
        XicException expected4 = new IntegerLiteralOutOfBoundsException(1, 1, "");

        String s5 = "-9223372036854775809";
        List<Symbol> expecteds5 = Arrays.asList(new Symbol(Sym.MINUS, 1, 1));
        XicException expected5 = new IntegerLiteralOutOfBoundsException(1, 2, "");

        String s6 = "13419223372036854775809";
        XicException expected6 = new IntegerLiteralOutOfBoundsException(1, 1, "");

        assertSymEquals(expecteds1, lex(s1));
        assertSymEquals(expecteds2, lex(s2));
        assertSymEquals(expecteds3, lex(s3));
        assertSymEquals(expected4,  lex(s4));
        assertSymEquals(new Actions.Lexed(expecteds5, Optional.of(expected5)),  lex(s5));
        assertSymEquals(expected6,  lex(s6));
    }

    @Test
    public void generalCommentTest() throws IOException {
        String s1 = "// :) ***I can put'_w/e_'I want h3r3*/....\n";
        String s2 = "//\n";
        String s3 = "////////nested comments? i am ron burgundy?\n";
        String s4 = "//carriage return test\r\nif\n";
        String s5 = "//carriage return test\rif\n";
        List<Symbol> expecteds45 = Arrays.asList(
                new Symbol(Sym.IF,2,1),
                eof
        );
        assertSymEquals(Arrays.asList(eof), lex(s1));
        assertSymEquals(Arrays.asList(eof), lex(s2));
        assertSymEquals(Arrays.asList(eof), lex(s3));
        assertSymEquals(expecteds45, lex(s4));
        assertSymEquals(expecteds45, lex(s5));
    }

    @Test
    public void tokensAndWhitespaceCommentTest() throws IOException {
        String s1 = "if// /div else while !-==\n";
        List<Symbol>  expecteds1 = Arrays.asList(
                new Symbol(Sym.IF, 1, 1),
                eof
        );
        String s2 = "//\f if else\f\n";
        /* Should lex as EOF is inserted by StringReader */
        String s3 = "//";
        assertSymEquals(expecteds1, lex(s1));
        assertSymEquals(Arrays.asList(eof), lex(s2));
        assertSymEquals(Arrays.asList(eof), lex(s3));
    }

    @Test
    public void stringCommentTest() throws IOException {
        String s1 = "//\"hello\"\n";
        String s2 = "//\'c\'\n";
        String s3 = "//\'multichar chars!!!!!\'\n";
        String s4 = "//\" messed up string\n";
        String s5 = "//\" 9223372036854775808293\n";
        /* Random whitespace characters!! */
        String s6 = "//\u2028\u2029  \u0085white \n";
        assertSymEquals(Arrays.asList(eof), lex(s1));
        assertSymEquals(Arrays.asList(eof), lex(s2));
        assertSymEquals(Arrays.asList(eof), lex(s3));
        assertSymEquals(Arrays.asList(eof), lex(s4));
        assertSymEquals(Arrays.asList(eof), lex(s5));
        assertSymEquals(Arrays.asList(eof), lex(s6));
    }


    @Test
    public void whitespaceTest() throws IOException {
        String s = "    \t\f     \t";
        assertSymEquals(Arrays.asList(eof), lex(s));
    }

    @Test
    public void identifierTest() throws IOException {
        //          00000000011111111112222222
        //          12345678901234567890123456
        String s = "these_are all identifiers'";
        List<Symbol> expecteds = Arrays.asList(
            new Symbol(Sym.ID, 1, 1, "these_are"),
            new Symbol(Sym.ID, 1, 11, "all"),
            new Symbol(Sym.ID, 1, 15, "identifiers'"),
            eof
        );

        assertSymEquals(expecteds, lex(s));
    }
}
