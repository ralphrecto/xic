package mjw297.ast;

import java.util.Optional;

public final class Array implements Type {
    public final Optional<Expr> size;
    public Array(Optional<Expr> size) {
        this.size = size;
    }
    public <R> R accept(TypeVisitor<R> v) {
        return v.visit(this);
    }
}
