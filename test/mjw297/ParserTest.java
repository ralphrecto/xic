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
    ////////////////////////////////////////////////////////////////////////////
    // Helper Functions
    ////////////////////////////////////////////////////////////////////////////
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
        return SymUtil.sym(type, PositionKiller.dummyPosition.row,
                           PositionKiller.dummyPosition.col);
    }

    private static Symbol sym(int type, Object value) {
        return SymUtil.sym(type, PositionKiller.dummyPosition.row,
                           PositionKiller.dummyPosition.col, value);
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

    public void exprTestHelper(List<Symbol> syms, Expr<Position> e) throws Exception {
        List<Symbol> symbols = new ArrayList<>();
        symbols.add(sym(ID, "main"));
        symbols.add(sym(LPAREN));
        symbols.add(sym(RPAREN));
        symbols.add(sym(LBRACE));
        symbols.add(sym(ID, "x"));
        symbols.add(sym(EQ));
        for (Symbol sym : syms) {
            symbols.add(sym);
        }
        symbols.add(sym(RBRACE));

        Program<Position> expected = program(
                l(),
                l(proc(id("main"), l(), l(asgn(id("x"), e))))
        );
        assertEquals(expected, parse(symbols));
    }

    ////////////////////////////////////////////////////////////////////////////
    // Abbreviations
    ////////////////////////////////////////////////////////////////////////////
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

    private static BinOp<Position> plus(Expr<Position> lhs, Expr<Position> rhs) {
        return binOp(BinOpCode.PLUS, lhs, rhs);
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

    private static BoolLiteral<Position> true_ () {
        return boolLiteral(true);
    }

    private static BoolLiteral<Position> false_ () {
        return boolLiteral(false);
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

    private static Block<Position> block (
        List<Stmt<Position>> ss,
        Optional<Expr<Position>> ret
    ) {
        return Block.of(PositionKiller.dummyPosition, ss, ret);
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

    ////////////////////////////////////////////////////////////////////////////
    // Abbreviations
    ////////////////////////////////////////////////////////////////////////////

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
            sym(NUM, 5l)
        );
        Stmt<Position> stmt = declAsgn(
            l(annotatedId(id("x"), num()),
              annotatedId(id("y"), num())),
            numLiteral(5l)
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
            sym(ID, "a"), sym(EQ), sym(NUM, 5l)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5l));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest2() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(ID, "a"),
                sym(LBRACKET),
                sym(NUM, 5l),
                sym(RBRACKET),
                sym(EQ),
                sym(NUM, 5l)
        );
        Stmt<Position> stmt = asgn(
            id("a"),
            numLiteral(5l)
        );
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest3() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(ID, "a"),
                sym(LBRACKET),
                sym(NUM, 5l),
                sym(RBRACKET),
                sym(EQ),
                sym(NUM, 5l)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5l));
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
                sym(NUM, 5l)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5l));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest5() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(STRING, "hello"),
                sym(LBRACKET),
                sym(INT, 5l),
                sym(RBRACKET),
                sym(EQ),
                sym(NUM, 5l)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5l));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest6() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(ID, "a"),
                sym(LBRACKET),
                sym(ID, "b"),
                sym(LBRACKET),
                sym(INT, 5l),
                sym(RBRACKET),
                sym(PLUS),
                sym(ID, "f"),
                sym(LPAREN),
                sym(RPAREN),
                sym(RBRACKET),
                sym(EQ),
                sym(NUM, 5l)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5l));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest7() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(STRING, "["),
                sym(LBRACKET),
                sym(STRING, "["),
                sym(LBRACKET),
                sym(NUM, 0l),
                sym(RBRACKET),
                sym(RBRACKET),
                sym(EQ),
                sym(NUM, 5l)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5l));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    public void asgnTest8() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(ID, "f"),
                sym(LPAREN),
                sym(ID, "b"),
                sym(LBRACKET),
                sym(NUM, 0l),
                sym(RBRACKET),
                sym(RPAREN),
                sym(EQ),
                sym(NUM, 5l)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5l));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    // while (true) _
    public void whileTest1() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(WHILE, "f"),
                sym(LPAREN),
                sym(TRUE),
                sym(RPAREN),
                sym(UNDERSCORE)
        );
        Stmt<Position> stmt = while_(true_(), decl(l(underscore())));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    // while (true) {_}
    public void whileTest2() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(WHILE),
                sym(LPAREN),
                sym(FALSE),
                sym(RPAREN),
                sym(LBRACE),
                sym(UNDERSCORE),
                sym(RBRACE)
        );
        Stmt<Position> stmt =
            while_(false_(),
                   block(l(decl(l(underscore()))), Optional.empty()));
        stmtTestHelper(symbols, stmt);
    }

    @Test
    // while (1) {_; _; _}
    public void whileTest3() throws Exception {
        List<Symbol> symbols = Arrays.asList(
                sym(WHILE),
                sym(LPAREN),
                sym(NUM, 1l),
                sym(RPAREN),
                sym(LBRACE),
                sym(UNDERSCORE),
                sym(SEMICOLON),
                sym(UNDERSCORE),
                sym(SEMICOLON),
                sym(UNDERSCORE),
                sym(SEMICOLON),
                sym(RBRACE)
        );
        Stmt<Position> stmt =
            while_(
                numLiteral(1l),
                block(l(
                    decl(l(underscore())),
                    decl(l(underscore())),
                    decl(l(underscore()))),
                    Optional.empty()
                )
            );
        stmtTestHelper(symbols, stmt);
    }

    private void binopHelper(Symbol s1, BinOpCode c) throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(NUM, 1l),
            s1,
            sym(NUM, 2l)
        );

        Expr<Position> e = binOp(c, numLiteral(1l), numLiteral(2l));
        exprTestHelper(symbols, e);
    }

    @Test
    public void binopTest1() throws Exception {
        binopHelper(sym(PLUS), BinOpCode.PLUS);
    }
    @Test
    public void binopTest2() throws Exception {
        binopHelper(sym(STAR), BinOpCode.STAR);
    }
    @Test
    public void binopTest3() throws Exception {
        binopHelper(sym(MINUS), BinOpCode.MINUS);
    }
    @Test
    public void binopTest4() throws Exception {
        binopHelper(sym(DIV), BinOpCode.DIV);
    }
    @Test
    public void binopTest5() throws Exception {
        binopHelper(sym(MOD), BinOpCode.MOD);
    }
    @Test
    public void binopTest6() throws Exception {
        binopHelper(sym(LT), BinOpCode.LT);
    }
    @Test
    public void binopTest7() throws Exception {
        binopHelper(sym(LTE), BinOpCode.LTE);
    }
    @Test
    public void binopTest8() throws Exception {
        binopHelper(sym(GTE), BinOpCode.GTE);
    }
    @Test
    public void binopTest9() throws Exception {
        binopHelper(sym(GT), BinOpCode.GT);
    }
    @Test
    public void binopTest10() throws Exception {
        binopHelper(sym(EQEQ), BinOpCode.EQEQ);
    }
    @Test
    public void binopTest11() throws Exception {
        binopHelper(sym(NEQ), BinOpCode.NEQ);
    }
    @Test
    public void binopTest12() throws Exception {
        binopHelper(sym(AMP), BinOpCode.AMP);
    }
    @Test
    public void binopTest13() throws Exception {
        binopHelper(sym(BAR), BinOpCode.BAR);
    }
    @Test
    public void binopTest14() throws Exception {
        binopHelper(sym(HIGHMULT), BinOpCode.HIGHMULT);
    }

    /**
     * Tests that {@code 1 op 2 op 3} == {@code (1 op 2) op 3}. You have to
     * pass in the same token twice; otherwise the parser complains about token
     * recycling.
     */
    private void assocHelper(Symbol s1, Symbol s2, BinOpCode c)
                 throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(NUM, 1l),
            s1,
            sym(NUM, 2l),
            s2,
            sym(NUM, 3l)
        );

        Expr<Position> e =
            binOp(c, binOp(c, numLiteral(1l), numLiteral(2l)), numLiteral(3l));
        exprTestHelper(symbols, e);
    }

    @Test
    // 1 op 2 op 3
    public void assocTest1() throws Exception {
        assocHelper(sym(PLUS), sym(PLUS), BinOpCode.PLUS);
    }
    @Test
    public void assocTest2() throws Exception {
        assocHelper(sym(STAR), sym(STAR), BinOpCode.STAR);
    }
    @Test
    public void assocTest3() throws Exception {
        assocHelper(sym(MINUS), sym(MINUS), BinOpCode.MINUS);
    }
    @Test
    public void assocTest4() throws Exception {
        assocHelper(sym(DIV), sym(DIV), BinOpCode.DIV);
    }
    @Test
    public void assocTest5() throws Exception {
        assocHelper(sym(MOD), sym(MOD), BinOpCode.MOD);
    }
    @Test
    public void assocTest6() throws Exception {
        assocHelper(sym(LT), sym(LT), BinOpCode.LT);
    }
    @Test
    public void assocTest7() throws Exception {
        assocHelper(sym(LTE), sym(LTE), BinOpCode.LTE);
    }
    @Test
    public void assocTest8() throws Exception {
        assocHelper(sym(GTE), sym(GTE), BinOpCode.GTE);
    }
    @Test
    public void assocTest9() throws Exception {
        assocHelper(sym(GT), sym(GT), BinOpCode.GT);
    }
    @Test
    public void assocTest10() throws Exception {
        assocHelper(sym(EQEQ), sym(EQEQ), BinOpCode.EQEQ);
    }
    @Test
    public void assocTest11() throws Exception {
        assocHelper(sym(NEQ), sym(NEQ), BinOpCode.NEQ);
    }
    @Test
    public void assocTest12() throws Exception {
        assocHelper(sym(AMP), sym(AMP), BinOpCode.AMP);
    }
    @Test
    public void assocTest13() throws Exception {
        assocHelper(sym(BAR), sym(BAR), BinOpCode.BAR);
    }
    @Test
    public void assocTest14() throws Exception {
        assocHelper(sym(HIGHMULT), sym(HIGHMULT), BinOpCode.HIGHMULT);
    }

    /**
     * Tests that {@code 1 op_1 2 op_2 3} == {@code 1 op_1 (2 op_2 3)} where
     * op_2 has higher precedence than op_1.
     */
    private void precHelper(Symbol op1, Symbol op2, BinOpCode c1, BinOpCode c2)
                 throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(NUM, 1l),
            op1,
            sym(NUM, 2l),
            op2,
            sym(NUM, 3l)
        );

        Expr<Position> e =
            binOp(c1, numLiteral(1l), binOp(c2, numLiteral(2l), numLiteral(3l)));
        exprTestHelper(symbols, e);
    }

    @Test
    public void precTest1() throws Exception {
        precHelper(sym(BAR), sym(AMP), BinOpCode.BAR, BinOpCode.AMP);
        precHelper(sym(BAR), sym(EQEQ), BinOpCode.BAR, BinOpCode.EQEQ);
        precHelper(sym(BAR), sym(NEQ), BinOpCode.BAR, BinOpCode.NEQ);
        precHelper(sym(BAR), sym(GT), BinOpCode.BAR, BinOpCode.GT);
        precHelper(sym(BAR), sym(GTE), BinOpCode.BAR, BinOpCode.GTE);
        precHelper(sym(BAR), sym(LT), BinOpCode.BAR, BinOpCode.LT);
        precHelper(sym(BAR), sym(LTE), BinOpCode.BAR, BinOpCode.LTE);
        precHelper(sym(BAR), sym(PLUS), BinOpCode.BAR, BinOpCode.PLUS);
        precHelper(sym(BAR), sym(MINUS), BinOpCode.BAR, BinOpCode.MINUS);
        precHelper(sym(BAR), sym(STAR), BinOpCode.BAR, BinOpCode.STAR);
        precHelper(sym(BAR), sym(DIV), BinOpCode.BAR, BinOpCode.DIV);
        precHelper(sym(BAR), sym(MOD), BinOpCode.BAR, BinOpCode.MOD);
        precHelper(sym(BAR), sym(HIGHMULT), BinOpCode.BAR, BinOpCode.HIGHMULT);
    }

    @Test
    public void precTest2() throws Exception {
        precHelper(sym(AMP), sym(EQEQ), BinOpCode.AMP, BinOpCode.EQEQ);
        precHelper(sym(AMP), sym(NEQ), BinOpCode.AMP, BinOpCode.NEQ);
        precHelper(sym(AMP), sym(GT), BinOpCode.AMP, BinOpCode.GT);
        precHelper(sym(AMP), sym(GTE), BinOpCode.AMP, BinOpCode.GTE);
        precHelper(sym(AMP), sym(LT), BinOpCode.AMP, BinOpCode.LT);
        precHelper(sym(AMP), sym(LTE), BinOpCode.AMP, BinOpCode.LTE);
        precHelper(sym(AMP), sym(PLUS), BinOpCode.AMP, BinOpCode.PLUS);
        precHelper(sym(AMP), sym(MINUS), BinOpCode.AMP, BinOpCode.MINUS);
        precHelper(sym(AMP), sym(STAR), BinOpCode.AMP, BinOpCode.STAR);
        precHelper(sym(AMP), sym(DIV), BinOpCode.AMP, BinOpCode.DIV);
        precHelper(sym(AMP), sym(MOD), BinOpCode.AMP, BinOpCode.MOD);
        precHelper(sym(AMP), sym(HIGHMULT), BinOpCode.AMP, BinOpCode.HIGHMULT);
    }

    @Test
    public void precTest3() throws Exception {
        precHelper(sym(EQEQ), sym(GT), BinOpCode.EQEQ, BinOpCode.GT);
        precHelper(sym(EQEQ), sym(GTE), BinOpCode.EQEQ, BinOpCode.GTE);
        precHelper(sym(EQEQ), sym(LT), BinOpCode.EQEQ, BinOpCode.LT);
        precHelper(sym(EQEQ), sym(LTE), BinOpCode.EQEQ, BinOpCode.LTE);
        precHelper(sym(EQEQ), sym(PLUS), BinOpCode.EQEQ, BinOpCode.PLUS);
        precHelper(sym(EQEQ), sym(MINUS), BinOpCode.EQEQ, BinOpCode.MINUS);
        precHelper(sym(EQEQ), sym(STAR), BinOpCode.EQEQ, BinOpCode.STAR);
        precHelper(sym(EQEQ), sym(DIV), BinOpCode.EQEQ, BinOpCode.DIV);
        precHelper(sym(EQEQ), sym(MOD), BinOpCode.EQEQ, BinOpCode.MOD);
        precHelper(sym(EQEQ), sym(HIGHMULT), BinOpCode.EQEQ, BinOpCode.HIGHMULT);
    }

    @Test
    public void precTest4() throws Exception {
        precHelper(sym(NEQ), sym(GT), BinOpCode.NEQ, BinOpCode.GT);
        precHelper(sym(NEQ), sym(GTE), BinOpCode.NEQ, BinOpCode.GTE);
        precHelper(sym(NEQ), sym(LT), BinOpCode.NEQ, BinOpCode.LT);
        precHelper(sym(NEQ), sym(LTE), BinOpCode.NEQ, BinOpCode.LTE);
        precHelper(sym(NEQ), sym(PLUS), BinOpCode.NEQ, BinOpCode.PLUS);
        precHelper(sym(NEQ), sym(MINUS), BinOpCode.NEQ, BinOpCode.MINUS);
        precHelper(sym(NEQ), sym(STAR), BinOpCode.NEQ, BinOpCode.STAR);
        precHelper(sym(NEQ), sym(DIV), BinOpCode.NEQ, BinOpCode.DIV);
        precHelper(sym(NEQ), sym(MOD), BinOpCode.NEQ, BinOpCode.MOD);
        precHelper(sym(NEQ), sym(HIGHMULT), BinOpCode.NEQ, BinOpCode.HIGHMULT);
    }

    @Test
    public void precTest5() throws Exception {
        precHelper(sym(GT), sym(PLUS), BinOpCode.GT, BinOpCode.PLUS);
        precHelper(sym(GT), sym(MINUS), BinOpCode.GT, BinOpCode.MINUS);
        precHelper(sym(GT), sym(STAR), BinOpCode.GT, BinOpCode.STAR);
        precHelper(sym(GT), sym(DIV), BinOpCode.GT, BinOpCode.DIV);
        precHelper(sym(GT), sym(MOD), BinOpCode.GT, BinOpCode.MOD);
        precHelper(sym(GT), sym(HIGHMULT), BinOpCode.GT, BinOpCode.HIGHMULT);
    }

    @Test
    public void precTest6() throws Exception {
        precHelper(sym(GTE), sym(PLUS), BinOpCode.GTE, BinOpCode.PLUS);
        precHelper(sym(GTE), sym(MINUS), BinOpCode.GTE, BinOpCode.MINUS);
        precHelper(sym(GTE), sym(STAR), BinOpCode.GTE, BinOpCode.STAR);
        precHelper(sym(GTE), sym(DIV), BinOpCode.GTE, BinOpCode.DIV);
        precHelper(sym(GTE), sym(MOD), BinOpCode.GTE, BinOpCode.MOD);
        precHelper(sym(GTE), sym(HIGHMULT), BinOpCode.GTE, BinOpCode.HIGHMULT);
    }

    @Test
    public void precTest7() throws Exception {
        precHelper(sym(LT), sym(PLUS), BinOpCode.LT, BinOpCode.PLUS);
        precHelper(sym(LT), sym(MINUS), BinOpCode.LT, BinOpCode.MINUS);
        precHelper(sym(LT), sym(STAR), BinOpCode.LT, BinOpCode.STAR);
        precHelper(sym(LT), sym(DIV), BinOpCode.LT, BinOpCode.DIV);
        precHelper(sym(LT), sym(MOD), BinOpCode.LT, BinOpCode.MOD);
        precHelper(sym(LT), sym(HIGHMULT), BinOpCode.LT, BinOpCode.HIGHMULT);
    }

    @Test
    public void precTest8() throws Exception {
        precHelper(sym(LTE), sym(PLUS), BinOpCode.LTE, BinOpCode.PLUS);
        precHelper(sym(LTE), sym(MINUS), BinOpCode.LTE, BinOpCode.MINUS);
        precHelper(sym(LTE), sym(STAR), BinOpCode.LTE, BinOpCode.STAR);
        precHelper(sym(LTE), sym(DIV), BinOpCode.LTE, BinOpCode.DIV);
        precHelper(sym(LTE), sym(MOD), BinOpCode.LTE, BinOpCode.MOD);
        precHelper(sym(LTE), sym(HIGHMULT), BinOpCode.LTE, BinOpCode.HIGHMULT);
    }


    @Test
    public void precTest9() throws Exception {
        precHelper(sym(PLUS), sym(STAR), BinOpCode.PLUS, BinOpCode.STAR);
        precHelper(sym(PLUS), sym(DIV), BinOpCode.PLUS, BinOpCode.DIV);
        precHelper(sym(PLUS), sym(MOD), BinOpCode.PLUS, BinOpCode.MOD);
        precHelper(sym(PLUS), sym(HIGHMULT), BinOpCode.PLUS, BinOpCode.HIGHMULT);
    }

    @Test
    public void precTest10() throws Exception {
        precHelper(sym(MINUS), sym(STAR), BinOpCode.MINUS, BinOpCode.STAR);
        precHelper(sym(MINUS), sym(DIV), BinOpCode.MINUS, BinOpCode.DIV);
        precHelper(sym(MINUS), sym(MOD), BinOpCode.MINUS, BinOpCode.MOD);
        precHelper(sym(MINUS), sym(HIGHMULT), BinOpCode.MINUS, BinOpCode.HIGHMULT);
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
