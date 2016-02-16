package mjw297.ast;

public interface VarVisitor<R> {
    public R visit(AnnotatedVar v);
    public R visit(Underscore u);
}
