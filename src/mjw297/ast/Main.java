package mjw297.ast;

import java.util.Arrays;

public class Main {
    private static class ClassPrinter implements NodeVisitor<String> {
        public String visit(AnnotatedId i) {
            return String.format("AnnotatedId(%s, %s)",
                    i.x.accept(this), i.t.accept(this));
        }
        public String visit(AnnotatedUnderscore u) {
            return String.format("AnnotatedUnderscore(%s, %s)",
                    u.u.accept(this), u.t.accept(this));
        }
        public String visit(Array a) {
            if (a.size.isPresent()) {
                return String.format("Array(%s, Some (%s))",
                        a.t.accept(this), a.size.get().accept(this));
            } else {
                return String.format("Array(%s, None)",
                        a.t.accept(this));
            }
        }
        public String visit(ArrayLiteral a) {
            String s = "";
            s += "ArrayLiteral(";
            for (Expr e : a.xs) {
                s += e.accept(this);
                s += ", ";
            }
            s += ")";
            return s;
        }
        public String visit(Asgn a) {
            return "Asgn";
        }
        public String visit(BinOp b) {
            return String.format("Binop(%s, %s, %s)",
                    b.c.toString(), b.lhs.accept(this), b.rhs.accept(this));
        }
        public String visit(Bool b) {
            return "Bool";
        }
        public String visit(BoolLiteral b) {
            return String.format("BoolLiteral(%b)", b.b);
        }
        public String visit(Call c) {
            String s = "";
            for (Expr e : c.args) {
                s += e.accept(this);
                s += ", ";
            }
            return String.format("Call(%s(%s))", c.f.accept(this), s);
        }
        public String visit(CharLiteral c) {
            return String.format("CharLiteral(%c)", c.c);
        }
        public String visit(DeclAsgn d) {
            return "TODO";
        }
        public String visit(Decl d) {
            return "TODO";
        }
        public String visit(Func f) {
            return "TODO";
        }
        public String visit(Id i) {
            return "TODO";
        }
        public String visit(IfElse i) {
            return "TODO";
        }
        public String visit(If i) {
            return "TODO";
        }
        public String visit(Index i) {
            return "TODO";
        }
        public String visit(Int i) {
            return "TODO";
        }
        public String visit(Length l) {
            return "TODO";
        }
        public String visit(NumLiteral n) {
            return "TODO";
        }
        public String visit(ParenthesizedExpr p) {
            return "TODO";
        }
        public String visit(Proc p) {
            return "TODO";
        }
        public String visit(Program p) {
            return "TODO";
        }
        public String visit(StringLiteral s) {
            return "TODO";
        }
        public String visit(Underscore u) {
            return "TODO";
        }
        public String visit(UnOp u) {
            return "TODO";
        }
        public String visit(Use u) {
            return "TODO";
        }
        public String visit(While w) {
            return "TODO";
        }
    }

    public static void main(String[] args) {
        Expr e = new ArrayLiteral(Arrays.asList(
            new CharLiteral('a'),
            new CharLiteral('b'),
            new BoolLiteral(true),
            new BoolLiteral(false)
        ));
        System.out.println(e.accept(new ClassPrinter()));
    }
}
