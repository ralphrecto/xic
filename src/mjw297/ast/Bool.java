package mjw297.ast;

public final class Bool implements Type {
    public <R> R accept(TypeVisitor<R> v) {
        return v.visit(this);
    }
}
