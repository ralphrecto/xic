package mjw297.ast;

public final class AnnotatedId implements AnnotatedVar {
    public final Id x;
    public final Type t;
    public AnnotatedId(Id x, Type t) {
        this.x = x;
        this.t = t;
    }
    public <R> R accept(AnnotatedVarVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
