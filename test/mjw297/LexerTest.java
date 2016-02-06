package mjw297;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java_cup.runtime.*;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class LexerTest {
    private void assertSymEquals(List<Symbol> expecteds, List<Symbol> actuals) {
        assertEquals(expecteds.size(), actuals.size());
        for (int i = 0; i < expecteds.size(); ++i) {
            Symbol expected = actuals.get(i);
            Symbol actual = actuals.get(i);
            assertEquals(expected.sym, actual.sym);
            assertEquals(expected.value, actual.value);
        }
    }

    @Test
    public void symbolEqualsTest() throws IOException {
        Symbol a = new Symbol(Sym.EQEQ);
        Symbol b = new Symbol(Sym.EQEQ);
        assertSymEquals(Arrays.asList(a), Arrays.asList(b));
    }
}
