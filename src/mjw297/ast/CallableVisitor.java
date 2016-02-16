package mjw297.ast;

public interface CallableVisitor<R> {
    public R visit(Func f);
    public R visit(Proc p);
}
