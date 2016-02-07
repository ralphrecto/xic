package mjw297;

import java.io.StringReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java_cup.runtime.*;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class LexerTest {

	private Symbol eof = new Symbol(Sym.EOF, -1, -1);

	private void assertSymEquals(Symbol expected, Symbol actual) {
		assertSymEquals(Arrays.asList(expected), Arrays.asList(actual));
	}

	// TODO: check rows and cols
	
    private void assertSymEquals(List<Symbol> expecteds, List<Symbol> actuals) {
        assertEquals("Error: number of tokens not equal.", expecteds.size(), actuals.size());
        for (int i = 0; i < expecteds.size(); ++i) {
            Symbol expected = expecteds.get(i);
            Symbol actual = actuals.get(i);
            assertEquals("Error: symbol codes not equal.", expected.sym, actual.sym);
            assertEquals("Error: symbol values not equal.", expected.value, actual.value);
			assertEquals("Error: symbol left index not equal.", expected.left, actual.left);
			assertEquals("Error: symbol right index not equal.", expected.right, actual.right);
        }
    }
	
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

    @Test
    public void symbolEqualsTest() throws IOException {
        Symbol a = new Symbol(Sym.EQEQ);
        Symbol b = new Symbol(Sym.EQEQ);
        assertSymEquals(Arrays.asList(a), Arrays.asList(b));
    }

	@Test
	public void keywordTest() throws IOException {
		Lexer  l = new Lexer(new StringReader("while"));
		Symbol s = l.next_token();
		Symbol expected = new Symbol(Sym.WHILE, 1, 1);
        assertSymEquals(Arrays.asList(expected), Arrays.asList(s));
	}

	@Test
	public void eofTest() throws IOException {
		Lexer  l = new Lexer(new StringReader(""));
		Symbol s = l.next_token();
		Symbol expected = new Symbol(Sym.EOF, -1, -1);
        assertSymEquals(Arrays.asList(expected), Arrays.asList(s));
	}

	@Test
	public void keywordsTest() throws IOException {
		//          000000000111111111122222222223333333333444
		//          123456789012345678901234567890123456789012
		String s = "whileifelsereturnintbooluselengthtruefalse";
		List<Symbol> expecteds = Arrays.asList(
			new Symbol(Sym.WHILE , 1, 1),
			new Symbol(Sym.IF    , 1, 6),
			new Symbol(Sym.ELSE  , 1, 8),
			new Symbol(Sym.RETURN, 1, 12),
			new Symbol(Sym.INT   , 1, 18), 
			new Symbol(Sym.BOOL  , 1, 21),
			new Symbol(Sym.USE   , 1, 25),
			new Symbol(Sym.LENGTH, 1, 28),
			new Symbol(Sym.TRUE  , 1, 34),
			new Symbol(Sym.FALSE , 1, 38),
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
	public void stringTest() throws IOException {
		Lexer  l = new Lexer(new StringReader("\"hello\t\""));
		Symbol s = l.next_token();
		Symbol expected = new Symbol(Sym.STRING, 1, 8, "hello\t");
    
	    assertSymEquals(expected, s);
	}

	@Test
	public void stringHexTest() throws IOException {
		Lexer  l = new Lexer(new StringReader("\"\\u000F\""));
		Symbol s = l.next_token();
		Symbol expected = new Symbol(Sym.STRING, 1, 2, "15");
    
	    assertSymEquals(expected, s);
	}

}
