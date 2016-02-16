package mjw297.ast;

public final class BoolLiteral implements Literal {
    public final boolean b;
    public BoolLiteral(boolean b) {
        this.b = b;
    }
    public <R> R accept(LiteralVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
