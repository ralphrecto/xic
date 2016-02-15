package mjw297.ast;

public interface LiteralVisitor<R> {
    public R visit(NumLiteral n);
    public R visit(BoolLiteral b);
    public R visit(StringLiteral s);
    public R visit(CharLiteral c);
    public R visit(ArrayLiteral a);
}
