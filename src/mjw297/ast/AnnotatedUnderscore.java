package mjw297.ast;

public final class AnnotatedUnderscore implements AnnotatedVar {
    public final Underscore u;
    public final Type t;
    public AnnotatedUnderscore(Underscore u, Type t) {
        this.u = u;
        this.t = t;
    }
    public <R> R accept(AnnotatedVarVisitor<R> v) {
        return v.visit(this);
    }
}
