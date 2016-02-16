package mjw297.ast;

public final class Asgn implements Stmt {
    // TODO: not sure
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
