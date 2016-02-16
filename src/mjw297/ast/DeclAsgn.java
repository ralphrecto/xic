package mjw297.ast;

import java.util.List;

public final class DeclAsgn implements Stmt {
    public final List<Var> vs;
    public final Expr e;
    public DeclAsgn(List<Var> vs, Expr e) {
        this.vs = vs;
        this.e = e;
    }
    public <R> R accept(StmtVisitor<R> v) {
        return v.visit(this);
    }
}
