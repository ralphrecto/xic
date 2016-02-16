package mjw297.ast;

public interface AnnotatedVarVisitor<R> {
    public R visit(AnnotatedId i);
    public R visit(AnnotatedUnderscore u);
}
