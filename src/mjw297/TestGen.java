package mjw297;

import com.google.common.collect.Lists;
import java_cup.runtime.Symbol;

import java.util.*;

import static mjw297.Ast.*;

public class TestGen {

    public static String tokenToLiteral(String token) {
        switch (token) {
            case ("EOF"):
                return "EOF";
            case ("error"):
                return "error";
            case ("MINUS"):
                return "-";
            case ("BANG"):
                return "!";
            case ("UMINUS"):
                return "-";
            case ("STAR"):
                return "*";
            case ("HIGHMULT"):
                return "*>>";
            case ("DIV"):
                return "/";
            case ("MOD"):
                return "%";
            case ("PLUS"):
                return "+";
            case ("EQ"):
                return "=";
            case ("LT"):
                return "<";
            case ("LTE"):
                return "<=";
            case ("GTE"):
                return ">=";
            case ("GT"):
                return ">";
            case ("EQEQ"):
                return "==";
            case ("NEQ"):
                return "!=";
            case ("AMP"):
                return "&";
            case ("BAR"):
                return "|";
            case ("SEMICOLON"):
                return ";";
            case ("LPAREN"):
                return "(";
            case ("RPAREN"):
                return ")";
            case ("LBRACKET"):
                return "[";
            case ("RBRACKET"):
                return "]";
            case ("LBRACE"):
                return "{";
            case ("RBRACE"):
                return "}";
            case ("UNDERSCORE"):
                return "_";
            case ("COMMA"):
                return ",";
            case ("COLON"):
                return ":";
            case ("WHILE"):
            case ("INT"):
            case ("BOOL"):
            case ("IF"):
            case ("ELSE"):
            case ("RETURN"):
            case ("USE"):
            case ("LENGTH"):
            case ("TRUE"):
            case ("FALSE"):
                return token.toLowerCase();
            default:
                return token.toLowerCase();
        }
    }

    private static final Random rand = new Random();


    static <T> List<T> repeat(java.util.concurrent.Callable<T> f, int times) {
        List<T> ret = new ArrayList<>();
        for (int i = 0; i < times; i++) {
            try {
                ret.add(f.call());
            } catch (Exception e) {
                return ret;
            }
        }
        return ret;
    }

    static <T> T choose(List<T> choices) {
        return choices.get(rand.nextInt(choices.size()));
    }

    static StringBuilder sbFlatten(List<StringBuilder> l, String delimiter) {
        StringBuilder ret = new StringBuilder();
        if (l.size() > 0) {
            for (int i = 0; i < l.size() - 1; i++) {
                ret.append(l.get(i));
                ret.append(delimiter);
             }
            ret.append(l.get(l.size() - 1));
        }
        return ret;
    }

    private static final int genInt(Integer limit) {
        return 1 + rand.nextInt(limit);
    }

    static String genString() {
        StringBuilder sb = new StringBuilder();
        int len = rand.nextInt(15);
        for (int i = 0; i < len + 1; i++) {
            char c = (char) (rand.nextInt(26) +
                (rand.nextBoolean() ? 'a' : 'A'));
            sb.append(c);
        }
        return sb.toString() + genInt(10);
    }

    static char genChar() {
        return (char) (rand.nextInt(26) + (rand.nextBoolean() ? 'a' : 'A'));
    }

    static Program genProgram() {
        return new Program(
            repeat(TestGen::genUse, genInt(15)),
            repeat(TestGen::genFunc, genInt(10))
        );
    }

    static Use genUse() {
        return new Use(genId());
    }

    static Callable genFunc() {
        return new Func(
            genId(),
            repeat(TestGen::genAVar, genInt(10)),
            repeat(TestGen::genType, genInt(5)),
            repeat(TestGen::genStmt, genInt(10)),
            repeat(TestGen::genExpr, genInt(5))
        );
    }

    static AnnotatedVar genAVar() {
        return new AnnotatedId(
            genId(),
            genType()
        );
    }

    static Underscore genUnderscore() {
        return new Underscore();
    }

    static Var genDeclVar() {
        switch (genInt(2)) {
            case 1: return genAVar();
            default: return genUnderscore();
        }
    }

    static Var genAsgnVar() {
        switch (genInt(2)) {
            case 1: return genId();
            default: return genUnderscore();
        }
    }

    static Int genIntType() {
        return new Int();
    }

    static Bool genBoolType() {
        return new Bool();
    }

    static Array genArrayType() {
        return new Array(genType(), Optional.empty());
    }

    static Type genType() {
        switch (genInt(3)) {
            case 1: return genIntType();
            case 2: return genBoolType();
            default: return genArrayType();
        }
    }

    /* Statements */

    static Decl genDecl() {
        return new Decl(
            repeat(TestGen::genDeclVar, genInt(6))
        );
    }

    static DeclAsgn genDeclAsgn() {
        return new DeclAsgn(
            repeat(TestGen::genDeclVar, genInt(6)),
            genExpr()
        );
    }

    static Asgn genAsgn() {
        return new Asgn(
            genId(),
            genExpr()
        );
    }

    static If genIf() {
        return new If(
            genExpr(),
            repeat(TestGen::genBaseStmt, genInt(10))
        );
    }

    static IfElse genIfElse() {
        return new IfElse(
            genExpr(),
            repeat(TestGen::genBaseStmt, genInt(10)),
            repeat(TestGen::genBaseStmt, genInt(10))
        );
    }

    static While genWhile() {
        return new While(
            genExpr(),
            repeat(TestGen::genBaseStmt, genInt(10))
        );
    }

    static Stmt genBaseStmt() {
        switch (genInt(3)) {
            case 1: return genDecl();
            case 2: return genDeclAsgn();
            default: return genAsgn();
        }
    }

    static Stmt genStmt() {
        if (rand.nextBoolean()) {
            return genBaseStmt();
        }
        switch (genInt(3)) {
            case 1: return genIf();
            case 2: return genIfElse();
            default: return genWhile();
        }
    }

    /* Expressions */

    static Id genId() {
        return new Id(genString());
    }

    static BinOp genBinOp() {
        BinOpCode[] binops = BinOpCode.values();
        return new BinOp(
            binops[rand.nextInt(binops.length)],
            genExpr(),
            genExpr()
        );
    }

    static UnOp genUnOp() {
        UnOpCode[] unops = UnOpCode.values();
        return new UnOp(
            unops[rand.nextInt(unops.length)],
            genExpr()
        );
    }

    static Length genLength() {
        return new Length(genExpr());
    }

    static NumLiteral genNumLit() {
        return new NumLiteral((long) genInt(Integer.MAX_VALUE));
    }

    static CharLiteral genCharLit() {
        return new CharLiteral(genChar());
    }

    static ArrayLiteral genArrayLit() {
        return new ArrayLiteral(repeat(TestGen::genBaseExpr, genInt(5)));
    }

    static StringLiteral genStringLit() {
        return new StringLiteral(genString());
    }

    static Expr genBaseExpr() {
        switch (genInt(4)) {
            case 1: return genId();
            case 2: return genNumLit();
            case 3: return genStringLit();
            default: return genCharLit();
        }
    }

    static Expr genExpr() {
        /* generate a base expr or a complex expr */
        if (rand.nextBoolean()) {
            return genBaseExpr();
        }

        switch (genInt(4)) {
            case 1: return genBinOp();
            case 2: return genUnOp();
            case 3: return genLength();
            default: return genArrayLit();
        }
    }

    public static class AstToProg implements NodeVisitor<StringBuilder> {

        @Override
        public StringBuilder visit(AnnotatedId i) {
            return new StringBuilder()
                    .append(i.x.accept(this))
                    .append(":")
                    .append(i.t.accept(this));
        }

        @Override
        public StringBuilder visit(AnnotatedUnderscore u) {
            return new StringBuilder()
                    .append(u.u.accept(this))
                    .append(":")
                    .append(u.t.accept(this));
        }

        @Override
        public StringBuilder visit(Func f) {
            return new StringBuilder()
                    .append(f.name.accept(this))
                    .append("(")
                    .append(sbFlatten(
                        Lists.transform(f.args, a -> a.accept(this)),
                        ", "
                    )).append("): ")
                    .append(sbFlatten(
                       Lists.transform(f.returnType, t -> t.accept(this)),
                        ", "
                    )).append(" {\n")
                    .append(sbFlatten(
                        Lists.transform(f.body, s -> s.accept(this)),
                        ";\n"
                    )).append(";\n")
                    .append("return ")
                    .append(sbFlatten(
                        Lists.transform(f.returns, e -> e.accept(this)),
                        ", "
                    )).append(";\n}\n");
        }

        @Override
        public StringBuilder visit(Proc p) {
            return new StringBuilder()
                    .append(p.accept(this))
                    .append("(")
                    .append(sbFlatten(
                            Lists.transform(p.args, a -> a.accept(this)),
                            ", "
                    )).append(") {\n")
                    .append(sbFlatten(
                            Lists.transform(p.body, s -> s.accept(this)),
                            ";\n"
                    )).append("\n}\n");
        }

        @Override
        public StringBuilder visit(Id i) {
            return new StringBuilder(i.x);
        }

        @Override
        public StringBuilder visit(BinOp o) {
            return new StringBuilder("(")
                    .append(o.lhs.accept(this))
                    .append(tokenToLiteral(o.c.name()))
                    .append(o.rhs.accept(this))
                    .append(")") ;
        }

        @Override
        public StringBuilder visit(UnOp o) {
            return new StringBuilder("(")
                    .append(tokenToLiteral(o.c.name()))
                    .append(o.e.accept(this))
                    .append(")");
        }

        @Override
        public StringBuilder visit(Index i) {
            return new StringBuilder("[[index]]");
        }

        @Override
        public StringBuilder visit(Length l) {
            return new StringBuilder("length(")
                    .append(l.e.accept(this))
                    .append(")");
        }

        @Override
        public StringBuilder visit(NumLiteral n) {
            return new StringBuilder((new Long(n.x)).toString());
        }

        @Override
        public StringBuilder visit(BoolLiteral b) {
            return new StringBuilder((new Boolean(b.b)).toString());
        }

        @Override
        public StringBuilder visit(StringLiteral s) {
            return new StringBuilder("\"" + s.s + "\"");
        }

        @Override
        public StringBuilder visit(CharLiteral c) {
            return new StringBuilder("\'" + c.c + "\'");
        }

        @Override
        public StringBuilder visit(ArrayLiteral a) {
            return new StringBuilder("{")
                    .append(sbFlatten(
                        Lists.transform(a.xs, e -> e.accept(this)),
                        ", "
                    )).append("}");
        }

        @Override
        public StringBuilder visit(Program p) {
            return new StringBuilder()
                    .append(sbFlatten(
                        Lists.transform(p.uses, u -> u.accept(this)),
                        "\n"
                    )).append("\n")
                    .append(sbFlatten(
                        Lists.transform(p.fs, f -> f.accept(this)),
                        "\n"
                    ));
        }

        @Override
        public StringBuilder visit(Decl d) {
            return new StringBuilder()
                    .append(sbFlatten(
                        Lists.transform(d.vs, v -> v.accept(this)),
                        ", "
                    ));
        }

        @Override
        public StringBuilder visit(DeclAsgn d) {
            return new StringBuilder()
                    .append(sbFlatten(
                            Lists.transform(d.vs, v -> v.accept(this)),
                            ", "
                    )).append(" = ")
                    .append(d.e.accept(this));
        }

        @Override
        public StringBuilder visit(Asgn a) {
            return new StringBuilder()
                    .append(a.id.accept(this))
                    .append(" = ")
                    .append(a.expr.accept(this));
        }

        @Override
        public StringBuilder visit(AsgnArrayIndex a) {
            return new StringBuilder()
                    .append(a.id.accept(this))
                    .append(sbFlatten(
                       Lists.transform(a.index, i -> new StringBuilder()
                               .append("[")
                               .append(i.accept(this))
                               .append("]")),
                        ""
                    )).append(" = ")
                    .append(a.expr.accept(this));
        }

        @Override
        public StringBuilder visit(If i) {
            return new StringBuilder("if (")
                    .append(i.b.accept(this))
                    .append(") {\n")
                    .append(sbFlatten(
                        Lists.transform(i.body, s -> s.accept(this)),
                        ";\n"
                    )).append("\n}");
        }

        @Override
        public StringBuilder visit(IfElse i) {
            return new StringBuilder("if (")
                    .append(i.b.accept(this))
                    .append(") {\n")
                    .append(sbFlatten(
                            Lists.transform(i.thenBody, s -> s.accept(this)),
                            ";\n"
                    )).append("\n} else {\n")
                    .append(sbFlatten(
                            Lists.transform(i.elseBody, s -> s.accept(this)),
                            ";\n"
                    )).append("\n}");
        }

        @Override
        public StringBuilder visit(While w) {
            return new StringBuilder("while (")
                    .append(w.b.accept(this))
                    .append(") {\n")
                    .append(sbFlatten(
                        Lists.transform(w.body, s -> s.accept(this)),
                        ";\n"
                    )).append("\n}");
        }

        @Override
        public StringBuilder visit(Int l) {
            return new StringBuilder("int");
        }

        @Override
        public StringBuilder visit(Bool o) {
            return new StringBuilder("bool");
        }

        @Override
        public StringBuilder visit(Array o) {
            return new StringBuilder(o.t.accept(this))
                    .append("[]");
        }

        @Override
        public StringBuilder visit(Use u) {
            return new StringBuilder("use ")
                    .append(u.x.accept(this));
        }

        @Override
        public StringBuilder visit(Underscore u) {
            return new StringBuilder("_");
        }

        @Override
        public StringBuilder visit(Call c) {
            return new StringBuilder(c.f.accept(this))
                    .append(sbFlatten(
                        Lists.transform(c.args, a -> a.accept(this)),
                        ", "
                    ));
        }
    }

}
