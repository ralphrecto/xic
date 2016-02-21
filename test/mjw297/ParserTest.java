package mjw297;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Optional;
import java.util.List;
import java_cup.runtime.Symbol;
import org.junit.Test;
import org.junit.Ignore;
import static mjw297.Ast.*;
import static mjw297.Sym.*;
import static org.junit.Assert.assertEquals;

public class ParserTest {
    @SuppressWarnings("deprecation")
    private static Program<Position> parsePos(List<Symbol> symbols) throws Exception {
        MockLexer l = new MockLexer(symbols) ;
        Parser p = new Parser(l);
        return p.parse().value();
    }

    @SuppressWarnings("deprecation")
    private static Program<Position> parse(List<Symbol> symbols) throws Exception {
        MockLexer l = new MockLexer(symbols) ;
        Parser p = new Parser(l);
        Program<Position> prog = p.parse().value();
        return PositionKiller.kill(prog);
    }

    @SafeVarargs
    private static <A> List<A> l(A... xs) {
        return Arrays.asList(xs);
    }

    private static Symbol sym(int type) {
        return SymUtil.sym(type, PositionKiller.dummyPosition.row, PositionKiller.dummyPosition.col);
    }

    private static Symbol sym(int type, Object value) {
        return SymUtil.sym(type, PositionKiller.dummyPosition.row, PositionKiller.dummyPosition.col, value);
    }

    private static Symbol sym(int type, int row, int col) {
        return SymUtil.sym(type, row, col);
    }

    private static Symbol sym(int type, Object value, int row, int col) {
        return SymUtil.sym(type, row, col, value);
    }

    private static Position pos(int row, int col) {
        return new Position(row, col);
    }

    public void stmtTestHelper(List<Symbol> syms, Stmt<Position> stmt) throws Exception {
        List<Symbol> symbols = new ArrayList<>();
        symbols.add(sym(ID, "main"));
        symbols.add(sym(LPAREN));
        symbols.add(sym(RPAREN));
        symbols.add(sym(LBRACE));

        for (Symbol sym : syms) {
            symbols.add(sym);
        }
        symbols.add(sym(RBRACE));

        Program<Position> expected = program(
                l(),
                l(proc(id("main"), l(), l(stmt)))
        );
        assertEquals(expected, parse(symbols));
    }


    private static AnnotatedId<Position> annotatedId (
        Id<Position> x,
        Type<Position> t
    ) {
        return AnnotatedId.of(PositionKiller.dummyPosition, x, t);
    }

    private static AnnotatedUnderscore<Position> annotatedUnderscore (
        Underscore<Position> u,
        Type<Position> t
    ) {
        return AnnotatedUnderscore.of(PositionKiller.dummyPosition, u, t);
    }

    private static Func<Position> func (
        Id<Position> name,
        List<AnnotatedVar<Position>> args,
        List<Type<Position>> returnType,
        List<Stmt<Position>> body,
        List<Expr<Position>> returns
    ) {
        return Func.of(PositionKiller.dummyPosition, name, args, returnType, body, returns);
    }

    private static Proc<Position> proc (
        Id<Position> name,
        List<AnnotatedVar<Position>> args,
        List<Stmt<Position>> body
    ) {
        return Proc.of(PositionKiller.dummyPosition, name, args, body);
    }

    private static Id<Position> id (
        String x
    ) {
        return Id.of(PositionKiller.dummyPosition, x);
    }

    private static BinOp<Position> binOp (
        BinOpCode c,
        Expr<Position> lhs,
        Expr<Position> rhs
    ) {
        return BinOp.of(PositionKiller.dummyPosition, c, lhs, rhs);
    }

    private static UnOp<Position> unOp (
        UnOpCode c,
        Expr<Position> e
    ) {
        return UnOp.of(PositionKiller.dummyPosition, c, e);
    }

    private static Index<Position> index (
        Expr<Position> e,
        Expr<Position> index
    ) {
        return Index.of(PositionKiller.dummyPosition, e, index);
    }

    private static Length<Position> length (
        Expr<Position> e
    ) {
        return Length.of(PositionKiller.dummyPosition, e);
    }

    private static NumLiteral<Position> numLiteral (
        long x
    ) {
        return NumLiteral.of(PositionKiller.dummyPosition, x);
    }

    private static BoolLiteral<Position> boolLiteral (
        boolean b
    ) {
        return BoolLiteral.of(PositionKiller.dummyPosition, b);
    }

    private static StringLiteral<Position> stringLiteral (
        String s
    ) {
        return StringLiteral.of(PositionKiller.dummyPosition, s);
    }

    private static CharLiteral<Position> charLiteral (
        char c
    ) {
        return CharLiteral.of(PositionKiller.dummyPosition, c);
    }

    private static ArrayLiteral<Position> arrayLiteral (
        List<Expr<Position>> xs
    ) {
        return ArrayLiteral.of(PositionKiller.dummyPosition, xs);
    }

    private static Program<Position> program (
        List<Use<Position>> uses,
        List<Callable<Position>> fs
    ) {
        return Program.of(PositionKiller.dummyPosition, uses, fs);
    }

    private static Decl<Position> decl (
        List<Var<Position>> vs
    ) {
        return Decl.of(PositionKiller.dummyPosition, vs);
    }

    private static DeclAsgn<Position> declAsgn (
        List<Var<Position>> vs,
        Expr<Position> e
    ) {
        return DeclAsgn.of(PositionKiller.dummyPosition, vs, e);
    }

    private static Asgn<Position> asgn (
        Id<Position> id,
        Expr<Position> expr
    ) {
        return Asgn.of(PositionKiller.dummyPosition, id, expr);
    }

    private static If<Position> if_ (
        Expr<Position> b,
        Stmt<Position> body
    ) {
        return If.of(PositionKiller.dummyPosition, b, body);
    }

    private static IfElse<Position> ifElse (
        Expr<Position> b,
        Stmt<Position> thenBody,
        Stmt<Position> elseBody
    ) {
        return IfElse.of(PositionKiller.dummyPosition, b, thenBody, elseBody);
    }

    private static While<Position> while_ (
        Expr<Position> b,
        Stmt<Position> body
    ) {
        return While.of(PositionKiller.dummyPosition, b, body);
    }

    private static Int<Position> num (
    ) {
        return Int.of(PositionKiller.dummyPosition);
    }

    private static Bool<Position> bool (
    ) {
        return Bool.of(PositionKiller.dummyPosition);
    }

    private static Array<Position> array (
        Type<Position> t,
        Optional<Expr<Position>> size
    ) {
        return Array.of(PositionKiller.dummyPosition, t, size);
    }

    private static Use<Position> use (
        Id<Position> x
    ) {
        return Use.of(PositionKiller.dummyPosition, x);
    }

    private static Underscore<Position> underscore (
    ) {
        return Underscore.of(PositionKiller.dummyPosition);
    }

    private static Call<Position> call (
        Id<Position> f,
        List<Expr<Position>> args
    ) {
        return Call.of(PositionKiller.dummyPosition, f, args);
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
    public void singleUsePosTest() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(USE, 1, 1),
            sym(ID, "foo", 1, 5),
            sym(ID, "main", 2, 1),
            sym(LPAREN, 2, 5),
            sym(RPAREN, 2, 6),
            sym(LBRACE, 3, 1),
            sym(RBRACE, 3, 2)
        );
        Program<Position> expected = Program.of(
            pos(0, 5),
            l(Use.of(
                pos(1, 1),
                Id.of(pos(1, 5), "foo")
             )),
            l(Proc.of(
                pos(2, 1),
                Id.of(pos(2, 1), "main"),
                l(),
                l()
             ))
        );
        assertEquals(expected, parsePos(symbols));
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

    /* Declarations */
    // x:int = x == x
    @Test
    public void declTest1() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "x"), sym(COLON), sym(INT),
            sym(EQ),
            sym(ID, "x"), sym(EQEQ), sym(ID, "x")
        );
        Stmt<Position> stmt = declAsgn(
            l(annotatedId(id("x"), Int.of(PositionKiller.dummyPosition))),
            binOp(BinOpCode.EQEQ, id("x"), id("x"))
        );
              
        stmtTestHelper(symbols, stmt);
    }

    // x:int, y:bool = 5
    @Test
    public void declTest2() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "x"), sym(COLON), sym(INT), sym(COMMA),
            sym(ID, "y"), sym(COLON), sym(BOOL), sym(EQ),
            sym(NUM, 5)
        );
        Stmt<Position> stmt = declAsgn(
            l(annotatedId(id("x"), num()),
              annotatedId(id("y"), num())),
            numLiteral((long) 5)
        );

        stmtTestHelper(symbols, stmt);
    }

    // _:int[]
    @Test
    public void declTest3() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(UNDERSCORE), sym(COLON),
            sym(INT), sym(LBRACKET), sym(RBRACKET)
        );
        Stmt<Position> stmt = decl(
            l(annotatedUnderscore(underscore(), array(num(), Optional.empty())))
        );
        
        stmtTestHelper(symbols, stmt); 
    }

    // x:int, y:bool, z:int, x:int, y:bool, z:int, x:int
    @Test
    public void declTest4() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "x"), sym(COLON), sym(INT), sym(COMMA),
            sym(ID, "y"), sym(COLON), sym(BOOL), sym(COMMA),
            sym(ID, "z"), sym(COLON), sym(INT), sym(COMMA),
            sym(ID, "x"), sym(COLON), sym(INT), sym(COMMA),
            sym(ID, "y"), sym(COLON), sym(BOOL), sym(COMMA),
            sym(ID, "z"), sym(COLON), sym(INT), sym(COMMA),
            sym(ID, "x"), sym(COLON), sym(INT)
        );
        Stmt<Position> stmt = decl(
            l(annotatedId(id("x"), num()),
              annotatedId(id("y"), bool()),
              annotatedId(id("z"), num()),
              annotatedId(id("x"), num()),
              annotatedId(id("y"), bool()),
              annotatedId(id("z"), num()),
              annotatedId(id("x"), num()))
        );

        stmtTestHelper(symbols, stmt);
    }

    // x:bool[]
    @Test
    public void declTest5() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "x"), sym(COLON),
            sym(BOOL), sym(LBRACKET), sym(RBRACKET)
        );
        Stmt<Position> stmt = decl(
            l(annotatedId(id("x"), array(num(), Optional.empty())))
        );

        stmtTestHelper(symbols, stmt);
    }

    // _ = expr
    @Test
    public void declTest6() throws Exception {

    }

    // _:bool = expr
    @Test
    public void declTest7() throws Exception {

    }

    // _:bool, y:bool = expr

    // y:bool, _:bool = expr;

    // x:bool, _ = expr;

    // x:int[x]

            
    @Test
    public void asgnTest1() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "a"), sym(EQ), sym(NUM, 5)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest2() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(ID, "a"),
                sym(LBRACKET),
                sym(NUM, 5),
                sym(RBRACKET),
                sym(EQ),
                sym(NUM, 5)
        );
        Stmt<Position> stmt = asgn(
            id("a"),
            numLiteral(5)
        );
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest3() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(ID, "a"),
                sym(LBRACKET),
                sym(NUM, 5),
                sym(RBRACKET),
                sym(EQ),
                sym(NUM, 5)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest4() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(ID, "a"),
                sym(LBRACKET),
                sym(ID, "f"),
                sym(LPAREN),
                sym(RPAREN),
                sym(RBRACKET),
                sym(EQ),
                sym(NUM, 5)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest5() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(STRING, "hello"),
                sym(LBRACKET),
                sym(INT, 5),
                sym(RBRACKET),
                sym(EQ),
                sym(NUM, 5)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest6() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(ID, "a"),
                sym(LBRACKET),
                sym(ID, "b"),
                sym(LBRACKET),
                sym(INT, 5),
                sym(RBRACKET),
                sym(PLUS),
                sym(ID, "f"),
                sym(LPAREN),
                sym(RPAREN),
                sym(RBRACKET),
                sym(EQ),
                sym(NUM, 5)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest7() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(STRING, "["),
                sym(LBRACKET),
                sym(STRING, "["),
                sym(LBRACKET),
                sym(NUM, 0),
                sym(RBRACKET),
                sym(RBRACKET),
                sym(EQ),
                sym(NUM, 5)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest8() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(ID, "f"),
                sym(LPAREN),
                sym(ID, "b"),
                sym(LBRACKET),
                sym(NUM, 0),
                sym(RBRACKET),
                sym(RPAREN),
                sym(EQ),
                sym(NUM, 5)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5));
        stmtTestHelper(symbols, stmt);
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
