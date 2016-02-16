package mjw297.ast;

import java.util.Optional;

public final class Array implements Type {
    public final Type t;
    public final Optional<Expr> size;
    public Array(Type t, Optional<Expr> size) {
        this.t = t;
        this.size = size;
    }
    public <R> R accept(TypeVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
