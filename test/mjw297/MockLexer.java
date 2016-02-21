package mjw297;

import java_cup.runtime.Scanner;
import java_cup.runtime.Symbol;
import java.util.List;
import java.util.Iterator;

public class MockLexer implements Scanner {
    private Iterator<Symbol> symbols;

    public MockLexer(List<Symbol> symbols) {
        this.symbols = symbols.iterator();
    }

    @Override
    public Symbol next_token() {
        return symbols.hasNext() ? symbols.next() : null;
    }
}
