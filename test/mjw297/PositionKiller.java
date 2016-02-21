package mjw297;

import com.google.common.collect.Lists;
import java.util.List;
import java.util.Optional;
import mjw297.Ast.*;

public class PositionKiller {
    public static Position dummyPosition = new Position(-1, -1);

    public static class ExprKiller implements
                        ExprVisitor<Position, Expr<Position>> {
        public Expr<Position> visit(Id<Position> i) {
            return Id.of(dummyPosition, i.x);
        }
        public Expr<Position> visit(BinOp<Position> o) {
            return BinOp.of(dummyPosition, o.c, o.lhs.accept(this), o.rhs.accept(this));
        }
        public Expr<Position> visit(UnOp<Position> o) {
            return UnOp.of(dummyPosition, o.c, o.e.accept(this));
        }
        public Expr<Position> visit(Index<Position> i) {
            return Index.of(dummyPosition, i.e.accept(this), i.index.accept(this));
        }
        public Expr<Position> visit(Length<Position> l) {
            return Length.of(dummyPosition, l.e.accept(this));
        }
        public Expr<Position> visit(Call<Position> c) {
            List<Expr<Position>> args = Lists.transform(c.args, e -> e.accept(this));
            return Call.of(dummyPosition, kill(c.f), args);
        }
        public Expr<Position> visit(NumLiteral<Position> n) {
            return NumLiteral.of(dummyPosition, n.x);
        }
        public Expr<Position> visit(BoolLiteral<Position> b) {
            return BoolLiteral.of(dummyPosition, b.b);
        }
        public Expr<Position> visit(StringLiteral<Position> s) {
            return StringLiteral.of(dummyPosition, s.s);
        }
        public Expr<Position> visit(CharLiteral<Position> c) {
            return CharLiteral.of(dummyPosition, c.c);
        }
        public Expr<Position> visit(ArrayLiteral<Position> a) {
            List<Expr<Position>> xs = Lists.transform(a.xs, e -> e.accept(this));
            return ArrayLiteral.of(dummyPosition, xs);
        }
    }

    public static class VarKiller implements
                        VarVisitor<Position, Var<Position>> {
        public Var<Position> visit(AnnotatedVar<Position> v) {
            return v.accept(new AnnotatedVarKiller());
        }
        public Var<Position> visit(Underscore<Position> u) {
            return Underscore.of(dummyPosition);
        }
    }

    public static class StmtKiller implements
                        StmtVisitor<Position, Stmt<Position>> {
        public Stmt<Position> visit(Decl<Position> d) {
            List<Var<Position>> vs =
                Lists.transform(d.vs, v -> v.accept(new VarKiller()));
            return Decl.of(dummyPosition, vs);
        }
        public Stmt<Position> visit(DeclAsgn<Position> d) {
            List<Var<Position>> vs =
                Lists.transform(d.vs, v -> v.accept(new VarKiller()));
            return DeclAsgn.of(dummyPosition, vs, d.e.accept(new ExprKiller()));
        }
        public Stmt<Position> visit(Asgn<Position> a) {
            return Asgn.of(dummyPosition, a.lhs.accept(new ExprKiller()),
                                          a.rhs.accept(new ExprKiller()));
        }
        public Stmt<Position> visit(Block<Position> b) {
            List<Stmt<Position>> ss = Lists.transform(b.ss, s -> s.accept(this));
            Optional<Expr<Position>> ret = b.ret.map(e -> e.accept(new ExprKiller()));
            return Block.of(dummyPosition, ss, ret);
        }
        public Stmt<Position> visit(If<Position> i) {
            return If.of(dummyPosition, i.b.accept(new ExprKiller()),
                                        i.body.accept(this));
        }
        public Stmt<Position> visit(IfElse<Position> i) {
            return IfElse.of(dummyPosition, i.b.accept(new ExprKiller()),
                    i.thenBody.accept(this), i.elseBody.accept(this));
        }
        public Stmt<Position> visit(While<Position> w) {
            return While.of(dummyPosition, w.b.accept(new ExprKiller()),
                            w.body.accept(this));
        }
        public Stmt<Position> visit(Call<Position> c) {
            List<Expr<Position>> args =
                Lists.transform(c.args, e -> e.accept(new ExprKiller()));
            return Call.of(dummyPosition, kill(c.f), args);
        }
    }

    public static class TypeKiller implements
                        TypeVisitor<Position, Type<Position>> {
        public Type<Position> visit(Int<Position> l) {
            return Int.of(dummyPosition);
        }
        public Type<Position> visit(Bool<Position> o) {
            return Bool.of(dummyPosition);
        }
        public Type<Position> visit(Array<Position> o) {
            Optional<Expr<Position>> size = o.size.map(e -> e.accept(new ExprKiller()));
            Type<Position> t = o.t.accept(new TypeKiller());
            return Array.of(dummyPosition, t, size);
        }
    }

    public static class AnnotatedVarKiller implements
                 AnnotatedVarVisitor<Position, AnnotatedVar<Position>> {
        public AnnotatedVar<Position> visit(AnnotatedId<Position> i)  {
            return AnnotatedId.of(dummyPosition, kill(i.x), i.t);
        }
        public AnnotatedVar<Position> visit(AnnotatedUnderscore<Position> u) {
            return AnnotatedUnderscore.of(dummyPosition, u.u, u.t);
        }
    }

    public static class CallableKiller implements
                        CallableVisitor<Position, Callable<Position>> {
        public Callable<Position> visit(Func<Position> f) {
            List<AnnotatedVar<Position>> args =
                Lists.transform(f.args, a -> a.accept(new AnnotatedVarKiller()));
            List<Type<Position>> retType =
                Lists.transform(f.returnType, t -> t.accept(new TypeKiller()));
            List<Stmt<Position>> body =
                Lists.transform(f.body, s -> s.accept(new StmtKiller()));
            List<Expr<Position>> returns =
                Lists.transform(f.returns, e -> e.accept(new ExprKiller()));
            return Func.of(dummyPosition, kill(f.name), args, retType,
                           body, returns);
        }
        public Callable<Position> visit(Proc<Position> p) {
            return Proc.of(dummyPosition, p.name, p.args, p.body);
        }
    }

    public static Id<Position> kill(Id<Position> i) {
        return Id.of(dummyPosition, i.x);
    }

    public static Use<Position> kill(Use<Position> u) {
        return Use.of(dummyPosition, kill(u.x));
    }

    public static Program<Position> kill(Program<Position> p) {
        CallableKiller ck = new CallableKiller();
        List<Use<Position>> uses = Lists.transform(p.uses, PositionKiller::kill);
        List<Callable<Position>> fs = Lists.transform(p.fs, c -> c.accept(ck));
        return Program.of(dummyPosition, uses, fs);
    }
}
