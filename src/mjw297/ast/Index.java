package mjw297.ast;

public final class Index implements Expr {
    public final Expr e;
    public final Expr index;
    public Index(Expr e, Expr index) {
        this.e = e;
        this.index = index;
    }
    public <R> R accept(ExprVisitor<R> v) {
        return v.visit(this);
    }
}
