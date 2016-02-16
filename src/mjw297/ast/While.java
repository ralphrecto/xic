package mjw297.ast;

public final class While implements Stmt {
    public final Expr b;
    public final Stmt body;
    public While(Expr b, Stmt body) {
        this.b = b;
        this.body = body;
    }
    public <R> R accept(StmtVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
