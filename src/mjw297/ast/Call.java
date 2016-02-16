package mjw297.ast;

import java.util.List;

public final class Call implements Expr, Stmt {
    public final Id f;
    public final List<Expr> args;
    public Call(Id f, List<Expr> args) {
        this.f = f;
        this.args = args;
    }
    public <R> R accept(ExprVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(StmtVisitor<R> v) {
        return v.visit(this);
    }
}
