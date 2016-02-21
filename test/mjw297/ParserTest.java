package mjw297;

import java.util.Arrays;
import java.util.Optional;
import java.util.List;
import java_cup.runtime.Symbol;
import org.junit.Test;
import static mjw297.Ast.*;
import static mjw297.Sym.*;
import static org.junit.Assert.assertEquals;

public class ParserTest {
    @SuppressWarnings("deprecation")
    private static Program<Position> parse(List<Symbol> symbols) throws Exception {
        MockLexer l = new MockLexer(symbols) ;
        Parser p = new Parser(l);
        return p.parse().value();
    }

    @SafeVarargs
    private static <A> List<A> l(A... xs) {
        return Arrays.asList(xs);
    }

    private static Position dummyPosition = new Position(-1, -1);

    private static Symbol sym(int type) {
        return SymUtil.sym(type, dummyPosition.row, dummyPosition.col);
    }

    private static Symbol sym(int type, Object value) {
        return SymUtil.sym(type, dummyPosition.row, dummyPosition.col, value);
    }


    private static AnnotatedId<Position> annotatedId (
        Id<Position> x,
        Type<Position> t
    ) {
        return AnnotatedId.of(dummyPosition, x, t);
    }

    private static AnnotatedUnderscore<Position> annotatedUnderscore (
        Underscore<Position> u,
        Type<Position> t
    ) {
        return AnnotatedUnderscore.of(dummyPosition, u, t);
    }

    private static Func<Position> func (
        Id<Position> name,
        List<AnnotatedVar<Position>> args,
        List<Type<Position>> returnType,
        List<Stmt<Position>> body,
        List<Expr<Position>> returns
    ) {
        return Func.of(dummyPosition, name, args, returnType, body, returns);
    }

    private static Proc<Position> proc (
        Id<Position> name,
        List<AnnotatedVar<Position>> args,
        List<Stmt<Position>> body
    ) {
        return Proc.of(dummyPosition, name, args, body);
    }

    private static Id<Position> id (
        String x
    ) {
        return Id.of(dummyPosition, x);
    }

    private static BinOp<Position> binOp (
        BinOpCode c,
        Expr<Position> lhs,
        Expr<Position> rhs
    ) {
        return BinOp.of(dummyPosition, c, lhs, rhs);
    }

    private static UnOp<Position> unOp (
        UnOpCode c,
        Expr<Position> e
    ) {
        return UnOp.of(dummyPosition, c, e);
    }

    private static Index<Position> index (
        Expr<Position> e,
        Expr<Position> index
    ) {
        return Index.of(dummyPosition, e, index);
    }

    private static Length<Position> length (
        Expr<Position> e
    ) {
        return Length.of(dummyPosition, e);
    }

    private static NumLiteral<Position> numLiteral (
        long x
    ) {
        return NumLiteral.of(dummyPosition, x);
    }

    private static BoolLiteral<Position> boolLiteral (
        boolean b
    ) {
        return BoolLiteral.of(dummyPosition, b);
    }

    private static StringLiteral<Position> stringLiteral (
        String s
    ) {
        return StringLiteral.of(dummyPosition, s);
    }

    private static CharLiteral<Position> charLiteral (
        char c
    ) {
        return CharLiteral.of(dummyPosition, c);
    }

    private static ArrayLiteral<Position> arrayLiteral (
        List<Expr<Position>> xs
    ) {
        return ArrayLiteral.of(dummyPosition, xs);
    }

    private static Program<Position> program (
        List<Use<Position>> uses,
        List<Callable<Position>> fs
    ) {
        return Program.of(dummyPosition, uses, fs);
    }

    private static Decl<Position> decl (
        List<Var<Position>> vs
    ) {
        return Decl.of(dummyPosition, vs);
    }

    private static DeclAsgn<Position> declAsgn (
        List<Var<Position>> vs,
        Expr<Position> e
    ) {
        return DeclAsgn.of(dummyPosition, vs, e);
    }

    private static Asgn<Position> asgn (
        Id<Position> id,
        Expr<Position> expr
    ) {
        return Asgn.of(dummyPosition, id, expr);
    }

    private static If<Position> if_ (
        Expr<Position> b,
        Stmt<Position> body
    ) {
        return If.of(dummyPosition, b, body);
    }

    private static IfElse<Position> ifElse (
        Expr<Position> b,
        Stmt<Position> thenBody,
        Stmt<Position> elseBody
    ) {
        return IfElse.of(dummyPosition, b, thenBody, elseBody);
    }

    private static While<Position> while_ (
        Expr<Position> b,
        Stmt<Position> body
    ) {
        return While.of(dummyPosition, b, body);
    }

    private static Int<Position> num (
    ) {
        return Int.of(dummyPosition);
    }

    private static Bool<Position> bool (
    ) {
        return Bool.of(dummyPosition);
    }

    private static Array<Position> array (
        Type<Position> t,
        Optional<Expr<Position>> size
    ) {
        return Array.of(dummyPosition, t, size);
    }

    private static Use<Position> use (
        Id<Position> x
    ) {
        return Use.of(dummyPosition, x);
    }

    private static Underscore<Position> underscore (
    ) {
        return Underscore.of(dummyPosition);
    }

    private static Call<Position> call (
        Id<Position> f,
        List<Expr<Position>> args
    ) {
        return Call.of(dummyPosition, f, args);
    }

    @Test
    public void emptyMainTest() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "main"),
            sym(LPAREN),
            sym(RPAREN),
            sym(LBRACE),
            sym(RBRACE)
        );
        Program<Position> expected = program(l(), l(proc(id("main"), l(), l())));
        assertEquals(expected, parse(symbols));
    }

    @Test
    public void emptyFooTest() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "foo_bar'"),
            sym(LPAREN),
            sym(RPAREN),
            sym(LBRACE),
            sym(RBRACE)
        );
        Program<Position> expected = program(l(), l(proc(id("foo_bar'"), l(), l())));
        assertEquals(expected, parse(symbols));
    }

    @Test
    public void singleUseTest() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(USE),
            sym(ID, "foo"),
            sym(ID, "main"),
            sym(LPAREN),
            sym(RPAREN),
            sym(LBRACE),
            sym(RBRACE)
        );
        Program<Position> expected = program(
            l(use(id("foo"))),
            l(proc(id("main"), l(), l()))
        );
        assertEquals(expected, parse(symbols));
    }

    @Test
    public void multiUseTest() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(USE), sym(ID, "foo"),
            sym(USE), sym(ID, "bar"),
            sym(USE), sym(ID, "foo"),
            sym(USE), sym(ID, "bar"),
            sym(ID, "main"),
            sym(LPAREN),
            sym(RPAREN),
            sym(LBRACE),
            sym(RBRACE)
        );
        Program<Position> expected = program(
            l(
                use(id("foo")),
                use(id("bar")),
                use(id("foo")),
                use(id("bar"))
            ),
            l(proc(id("main"), l(), l()))
        );
        assertEquals(expected, parse(symbols));
    }

    @Test
    public void multiProcTest() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "foo"), sym(LPAREN), sym(RPAREN), sym(LBRACE), sym(RBRACE),
            sym(ID, "bar"), sym(LPAREN), sym(RPAREN), sym(LBRACE), sym(RBRACE),
            sym(ID, "baz"), sym(LPAREN), sym(RPAREN), sym(LBRACE), sym(RBRACE)
        );
        Program<Position> expected = program(
            l(),
            l(proc(id("foo"), l(), l()),
              proc(id("bar"), l(), l()),
              proc(id("baz"), l(), l()))
        );
        assertEquals(expected, parse(symbols));
    }

    // TODO: determine function returns
    // @Test
    // public void singleFuncTest() throws Exception {
    //     List<Symbol> symbols = Arrays.asList(
    //         sym(ID, "foo"),
    //         sym(LPAREN),
    //         sym(RPAREN),
    //         sym(COLON),
    //         sym(INT),
    //         sym(LBRACE),
    //         sym(RBRACE)
    //     );
    //     Program<Position> expected = program(
    //         l(),
    //         l(func(id("foo"), l(), l(num()), l(), l()))
    //     );
    //     assertEquals(expected, parse(symbols));
    // }
}
