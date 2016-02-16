package mjw297.ast;

public final class Use implements Node {
    public final Id x;
    public Use(Id x) {
        this.x = x;
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
