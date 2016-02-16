package mjw297.ast;

import java.util.List;

public final class Decl implements Stmt {
    public final List<Var> vs;
    public Decl(List<Var> vs) {
        this.vs = vs;
    }
    public <R> R accept(StmtVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
