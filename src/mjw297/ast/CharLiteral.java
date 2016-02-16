package mjw297.ast;

public final class CharLiteral implements Literal {
    public final char c;
    public CharLiteral(char c) {
        this.c = c;
    }
    public <R> R accept(LiteralVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
