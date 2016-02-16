package mjw297.ast;

public final class UnOp implements Expr {
    public final UnOpCode c;
    public final Expr e;
    public UnOp(UnOpCode c, Expr e) {
        this.c = c;
        this.e = e;
    }
    public <R> R accept(ExprVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
