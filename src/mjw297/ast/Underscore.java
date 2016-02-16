package mjw297.ast;

public final class Underscore implements Var {
    public Underscore() {}
    public <R> R accept(VarVisitor<R> v) {
        return v.visit(this);
    }
}
