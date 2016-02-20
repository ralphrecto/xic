package mjw297;

import com.google.common.collect.Lists;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import static mjw297.Ast.*;

public class TestGen {

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

    static String genString() {
        StringBuilder sb = new StringBuilder();
        int len = rand.nextInt(15);
        for (int i = 0; i < len + 1; i++) {
            char c = (char) (rand.nextInt(26) +
                (rand.nextBoolean() ? 'a' : 'A'));
            sb.append(c);
        }
        return sb.toString();
    }

    static Program genProgram() {
        return new Program(
            repeat(TestGen::genUse, 2),
            repeat(TestGen::genFunc, 2)
        );
    }

    static Use genUse() {
        return new Use(genId());
    }

    static Callable genFunc() {
        return new Func(
            genId(),
            repeat(TestGen::genAVar, 2),
            repeat(TestGen::genType, 2),
            repeat(TestGen::genStmt, 5),
            repeat(TestGen::genExpr, 2)
        );
    }

    static Id genId() {
        return new Id(genString());
    }

    static AnnotatedVar genAVar() {
        return new AnnotatedId(
            genId(),
            genType()
        );
    }

    static Var genVar() {
        return rand.nextBoolean() ?
            genAVar() :
            new Underscore();
    }

    static Type genType() {
        return new Int();
    }

    static Stmt genStmt() {
        return new DeclAsgn(
            repeat(TestGen::genVar, 2),
            genExpr()
        );
    }

    static Expr genExpr() {
        return new NumLiteral(100L);
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
            return new StringBuilder()
                    .append(o.lhs.accept(this))
                    .append("+")
                    .append(o.rhs.accept(this));
        }

        @Override
        public StringBuilder visit(UnOp o) {
            return new StringBuilder()
                    .append("-")
                    .append(o.e.accept(this));
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
        public StringBuilder visit(ParenthesizedExpr e) {
            return new StringBuilder("[[parenexpr]]");
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
