package mjw297.ast;

public final class IfElse implements Stmt {
    public final Expr b;
    public final Stmt thenBody;
    public final Stmt elseBody;
    public IfElse(Expr b, Stmt thenBody, Stmt elseBody) {
        this.b = b;
        this.thenBody = thenBody;
        this.elseBody = elseBody;
    }
    public <R> R accept(StmtVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
