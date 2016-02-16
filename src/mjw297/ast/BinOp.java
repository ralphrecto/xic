package mjw297.ast;

public final class BinOp implements Expr {
    public final BinOpCode c;
    public final Expr lhs;
    public final Expr rhs;
    public BinOp(BinOpCode c, Expr lhs, Expr rhs) {
        this.c = c;
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public <R> R accept(ExprVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
