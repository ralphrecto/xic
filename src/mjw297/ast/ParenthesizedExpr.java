package mjw297.ast;

public final class ParenthesizedExpr implements Expr {
    public final Expr e;
    public ParenthesizedExpr(Expr e) {
        this.e = e;
    }
    public <R> R accept(ExprVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
