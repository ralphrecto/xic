package mjw297.ast;

public final class Id implements Node {
    public final String x;
    public Id(String x) {
        this.x = x;
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
