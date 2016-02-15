package mjw297.ast;

public interface TypeVisitor<R> {
    public R visit(Int l);
    public R visit(Bool o);
    public R visit(Array o);
}
