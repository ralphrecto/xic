package mjw297.ast;

public final class StringLiteral implements Literal {
    public final String s;
    public StringLiteral(String s) {
        this.s = s;
    }
    public <R> R accept(LiteralVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
