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
            sym(NUM, new Long(5L))
        );
        Stmt<Position> stmt = declAsgn(
            l(annotatedId(id("x"), num()),
              annotatedId(id("y"), bool())),
            numLiteral(new Long(5L))
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
            l(annotatedId(id("x"), array(bool(), Optional.empty())))
        );

        stmtTestHelper(symbols, stmt);
    }

    // _ = expr
    @Test
    public void declTest6() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(UNDERSCORE), sym(EQ), sym(NUM, new Long(5L))
        );
        Stmt<Position> stmt = declAsgn(
            l(underscore()),
            numLiteral(new Long(5L))
        );

        stmtTestHelper(symbols, stmt);
    }

    // _:bool = expr
    @Test
    public void declTest7() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(UNDERSCORE), sym(COLON), sym(BOOL),
            sym(EQ), sym(NUM, new Long(5L))
        );
        Stmt<Position> stmt = declAsgn(
            l(annotatedUnderscore(underscore(), bool())),
            numLiteral(new Long(5L))
        );

        stmtTestHelper(symbols, stmt);
    }

    // _:bool, y:bool = expr
    @Test
    public void declTest8() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(UNDERSCORE), sym(COLON), sym(BOOL), sym(COMMA),
            sym(ID, "y"), sym(COLON), sym(BOOL), sym(EQ),
            sym(NUM, new Long(5L))
        );
        Stmt<Position> stmt = declAsgn(
            l(annotatedUnderscore(underscore(), bool()),
              annotatedId(id("y"), bool())),
            numLiteral(new Long(5L))
        );

        stmtTestHelper(symbols, stmt);
    }

    // y:bool, _:bool = expr;
    @Test
    public void declTest9() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "y"), sym(COLON), sym(BOOL), sym(COMMA),
            sym(UNDERSCORE), sym(COLON), sym(BOOL), sym(EQ),
            sym(NUM, new Long(5L))
        );
        Stmt<Position> stmt = declAsgn(
            l(annotatedId(id("y"), bool()),
              annotatedUnderscore(underscore(), bool())),
            numLiteral(new Long(5L))
        );

        stmtTestHelper(symbols, stmt);
    }

    // x:bool, _ = expr;
    @Test
    public void declTest10() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "x"), sym(COLON), sym(BOOL), sym(COMMA),
            sym(UNDERSCORE), sym(EQ),
            sym(NUM, new Long(5L))
        );
        Stmt<Position> stmt = declAsgn(
            l(annotatedId(id("x"), bool()),
              underscore()),
            numLiteral(new Long(5L))
        );

        stmtTestHelper(symbols, stmt);
    }

    // _:int;
    @Test
    public void declTest11() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(UNDERSCORE), sym(COLON), sym(INT),
            sym(SEMICOLON)
        );
        Stmt<Position> stmt = decl(
            l(annotatedUnderscore(underscore(), num())) 
        );

        stmtTestHelper(symbols, stmt);
    }

    // _:int, y:bool;
    @Test
    public void declTest12() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(UNDERSCORE), sym(COLON), sym(INT), sym(COMMA),
            sym(ID, "y"), sym(COLON), sym(BOOL),
            sym(SEMICOLON)
        );
        Stmt<Position> stmt = decl(
            l(annotatedUnderscore(underscore(), num()),
              annotatedId(id("y"), bool()))
        );

        stmtTestHelper(symbols, stmt);
    }

    // b:bool;
    @Test
    public void declTest13() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "b"), sym(COLON), sym(BOOL),
            sym(SEMICOLON)
        );
        Stmt<Position> stmt = decl(
            l(annotatedId(id("b"), bool()))
        );

        stmtTestHelper(symbols, stmt);
    }

    // x:int, _:int;
    @Test
    public void declTest14() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "x"), sym(COLON), sym(INT), sym(COMMA),
            sym(UNDERSCORE), sym(COLON), sym(INT)
        );
        Stmt<Position> stmt = decl(
            l(annotatedId(id("x"), num()),
              annotatedUnderscore(underscore(), num()))
        );

        stmtTestHelper(symbols, stmt);
    }

    // x:int, _;
    @Test
    public void declTest15() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "x"), sym(COLON), sym(INT), sym(COMMA),
            sym(UNDERSCORE), sym(SEMICOLON)
        );
        Stmt<Position> stmt = decl(
            l(annotatedId(id("x"), num()),
              underscore())
        );

        stmtTestHelper(symbols, stmt);
    }

    // _, _ = f()
    @Test
    public void declTest16() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(UNDERSCORE), sym(COMMA), sym(UNDERSCORE),
            sym(EQ), sym(ID, "f"), sym(LPAREN),
            sym(RPAREN)
        );
        Stmt<Position> stmt = declAsgn(
            l(underscore(),
              underscore()),
            call(id("f"), l())
        );

        stmtTestHelper(symbols, stmt);
    }

    // a:int[]
    @Test
    public void declTest17() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "a"), sym(COLON), sym(INT),
            sym(LBRACKET), sym(RBRACKET)
        );
        Stmt<Position> stmt = decl(
            l(annotatedId(id("a"), array(num(), Optional.empty())))
        );

        stmtTestHelper(symbols, stmt);
    }

    // a:int[][]
    @Test
    public void declTest18() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "a"), sym(COLON), sym(INT),
            sym(LBRACKET), sym(RBRACKET),
            sym(LBRACKET), sym(RBRACKET)
        );
        Stmt<Position> stmt = decl(
            l(annotatedId(id("a"), array(array(num(), Optional.empty()), Optional.empty())))
        );

        stmtTestHelper(symbols, stmt);
    }

    // a:int[n+5][f()];
    @Test
    public void declTest19() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "a"), sym(COLON), sym(INT),
            sym(LBRACKET), sym(ID, "n"), sym(PLUS),
            sym(NUM, new Long(5L)), sym(RBRACKET),
            sym(LBRACKET), sym(ID, "f"),
            sym(LPAREN), sym(RPAREN), sym(RBRACKET),
            sym(SEMICOLON)
        );
        Stmt<Position> stmt = decl(
            l(annotatedId(id("a"), array(
                array(
                    num(),
                    Optional.of(binOp(BinOpCode.PLUS, id("n"), numLiteral(new Long(5L))))),
                Optional.of(call(id("f"), l())))))
        );

        stmtTestHelper(symbols, stmt);
    }

    // a:int[5][];
    @Test
    public void declTest20() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "a"), sym(COLON), sym(INT),
            sym(LBRACKET), sym(NUM, 5L), 
            sym(RBRACKET),
            sym(LBRACKET), sym(RBRACKET),
            sym(SEMICOLON)
        );
        Stmt<Position> stmt = decl(
            l(annotatedId(id("a"), array(
                array(
                    num(),
                    Optional.of(numLiteral(5L))),
                Optional.empty())))

        );

        stmtTestHelper(symbols, stmt);
    }

    // a:int[] = {1, 2, 3};
    @Test
    public void declTest21() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "a"), sym(COLON), sym(INT),
            sym(LBRACKET), sym(RBRACKET), sym(EQ),
            sym(LBRACE), sym(NUM, 1L), sym(COMMA),
            sym(NUM, 2L), sym(COMMA),
            sym(NUM, 3L), sym(COMMA),
            sym(RBRACE), sym(SEMICOLON)
        );
        Stmt<Position> stmt = declAsgn(
            l(annotatedId(id("a"), array(num(), Optional.empty()))),
            arrayLiteral(Arrays.asList(
                numLiteral(new Long(1L)),
                numLiteral(new Long(2L)),
                numLiteral(new Long(3L))))
        );

        stmtTestHelper(symbols, stmt);
    }

    // a:int[][] = {{1, 2, 3}, {4}}
    @Test
    public void declTest22() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "a"), sym(COLON), sym(INT),
            sym(LBRACKET), sym(RBRACKET),
            sym(LBRACKET), sym(RBRACKET), sym(EQ),
            sym(LBRACE), sym(LBRACE),
            sym(NUM, new Long(1L)), sym(COMMA),
            sym(NUM, new Long(2L)), sym(COMMA),
            sym(NUM, new Long(3L)), sym(RBRACE),
            sym(COMMA), sym(LBRACE),
            sym(NUM, new Long(4L)),
            sym(RBRACE), sym(RBRACE)
        );
        Stmt<Position> stmt = declAsgn(
            l(annotatedId(id("a"), array(array(num(), Optional.empty()), Optional.empty()))),
            arrayLiteral(l(
                arrayLiteral(l(
                    numLiteral(new Long(1L)),
                    numLiteral(new Long(2L)),
                    numLiteral(new Long(3L)))),
                arrayLiteral(l(
                    numLiteral(new Long(4L))))))
        );

        stmtTestHelper(symbols, stmt);
    }

    // a:int[({1}[0])]
    @Test
    public void declTest23() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "a"), sym(COLON), sym(INT),
            sym(LBRACKET), sym(LPAREN),
            sym(LBRACE), sym(NUM, new Long(1L)),
            sym(RBRACE), sym(LBRACKET),
            sym(NUM, new Long(0L)), sym(RBRACKET),
            sym(RPAREN), sym(RBRACKET)
        );
        Stmt<Position> stmt = decl(
            l(annotatedId(id("a"), array(num(),
                Optional.of(index(
                    arrayLiteral(l(numLiteral(new Long(1L)))),
                    numLiteral(new Long(0L)))))))
        );

        stmtTestHelper(symbols, stmt);
    }

    // _, _, _, _
    @Test
    public void declTest24() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(UNDERSCORE), sym(COMMA),
            sym(UNDERSCORE), sym(COMMA),
            sym(UNDERSCORE), sym(COMMA),
            sym(UNDERSCORE)
        );
        Stmt<Position> stmt = decl(
            l(underscore(), underscore(), underscore(), underscore())
        );

        stmtTestHelper(symbols, stmt);
    }

    // x:int[x]
    @Test
    public void declTest25() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "x"), sym(COLON),
            sym(INT), sym(LBRACKET), sym(ID, "x"),
            sym(RBRACKET)
        );
        Stmt<Position> stmt = decl(
            l(annotatedId(id("x"), array(num(), Optional.of(id("x")))))
        );

        stmtTestHelper(symbols, stmt);
    }

    
    /* Assignments */
    @Test
    public void asgnTest1() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(ID, "a"), sym(EQ), sym(NUM, 5l)
        );
        Stmt<Position> stmt = asgn(id("a"), numLiteral(5l));
        stmtTestHelper(symbols, stmt);
    }

    @Ignore
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

    @Ignore
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

    @Ignore
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

    @Ignore
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

    @Ignore
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

    @Ignore
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

    @Ignore
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

    @Ignore
    @Test
    // 1 + 2 + 3
    public void assocTest1() throws Exception {
        List<Symbol> symbols = Arrays.asList(
            sym(NUM, 1l),
            sym(PLUS),
            sym(NUM, 2l),
            sym(PLUS),
            sym(NUM, 3l)
        );
        Expr<Position> e = plus(plus(numLiteral(1l), numLiteral(2l)), numLiteral(3l));
        exprTestHelper(symbols, e);
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
