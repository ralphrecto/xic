package mjw297;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java_cup.runtime.*;
import mjw297.XicException.*;
import org.junit.Ignore;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class LexerTest {
    /**
     * A {@code Lexed} instance represents the result of lexing a stream of
     * characters. Lexing results in a list of symbols and optionally an
     * exception.
     */
    private static class Lexed {
        public final List<Symbol> symbols;
        public final Optional<XicException> exception;

        public Lexed(List<Symbol> symbols, Optional<XicException> exception) {
            this.symbols = symbols;
            this.exception = exception;
        }
    }

    private Symbol eof = new Symbol(Sym.EOF, -1, -1);

    /**
     * {@code lex(s)} uses the {@code Lexer} class to repeatedly lex tokens
     * from {@code s} until the EOF token is reached. The EOF token <i>is</i>
     * included in the returned list.
     */
    private Lexed lex(String s) throws IOException {
        List<Symbol> result = new ArrayList<>();
        Lexer l = new Lexer(new StringReader(s));

        try {
            // Collect all tokens until EOF
            Symbol sym = l.next_token();
            while (sym.sym != Sym.EOF) {
                result.add(sym);
                sym = l.next_token();
            }
            result.add(sym);
            return new Lexed(result, Optional.empty());
        } catch (XicException e) {
            return new Lexed(result, Optional.of(e));
        }
    }

    /**
     * {@code assertSymEquals(e, a)} is shorthand for {@code
     * assertSymEquals(Lexed([e], None), Lexed([a], None))}
     */
    private void assertSymEquals(Symbol expected, Symbol actual) {
        assertSymEquals(new Lexed(Arrays.asList(expected), Optional.empty()),
                        new Lexed(Arrays.asList(actual), Optional.empty()));
    }

    /**
     * {@code assertSymEquals(e, a)} is shorthand for {@code
     * assertSymEquals(Lexed(e, None), Lexed(a, None))}
     */
    private void assertSymEquals(List<Symbol> expecteds, Lexed actual) {
        assertSymEquals(new Lexed(expecteds, Optional.empty()), actual);
    }

    /**
     * {@code assertSymEquals(e, a)} is shorthand for {@code
     * assertSymEquals(Lexed([], e), Lexed(a, None))}
     */
    private void assertSymEquals(XicException expected, Lexed actual) {
        assertSymEquals(new Lexed(Arrays.asList(), Optional.of(expected)), actual);
    }

    /**
     * {@code assertSymEquals(Lexed(e1..en, ee), Lexed(a1..am, ae)} asserts
     * that the {@code n == m}, {@code ee == ae}, and that for all {@code i} in
     * {@code 1..n}, {@code assertSymEquals(ei, ai)}.
     */
    private void assertSymEquals(Lexed expected, Lexed actual) {
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

    @Ignore
    @Test
    public void stringTest() throws IOException, XicException {
        Lexer  l = new Lexer(new StringReader("\"hello\t\""));
        Symbol s = l.next_token();
        Symbol expected = new Symbol(Sym.STRING, 1, 8, "hello\t");

        assertSymEquals(expected, s);
    }

    @Ignore
    @Test
    public void stringHexTest() throws IOException, XicException {
        Lexer  l = new Lexer(new StringReader("\"\\u000F\""));
        Symbol s = l.next_token();
        Symbol expected = new Symbol(Sym.STRING, 1, 2, "15");

        assertSymEquals(expected, s);
    }

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
        assertSymEquals(new Lexed(expecteds5, Optional.of(expected5)),  lex(s5));
        assertSymEquals(expected6,  lex(s6));
    }

    @Test
    public void commentTest() throws IOException {
        String s1 = "// :) ***I can put'_w/e_'I want h3r3....\\f\\r */\n";
        String s2 = "//\n";
        assertSymEquals(Arrays.asList(eof), lex(s1));
        assertSymEquals(Arrays.asList(eof), lex(s2));
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
