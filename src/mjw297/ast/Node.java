package mjw297.ast;

public interface Node {
    public <R> R accept(NodeVisitor<R> v);
}
