package mjw297.ast;

public final class NumLiteral implements Literal {
    public final long x;
    public NumLiteral(long x) {
        this.x = x;
    }
    public <R> R accept(LiteralVisitor<R> v) {
        return v.visit(this);
    }
}
