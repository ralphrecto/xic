package mjw297.ast;

import java.util.List;

public final class ArrayLiteral implements Literal {
    public final List<Expr> xs;
    public ArrayLiteral(List<Expr> xs) {
        this.xs = xs;
    }
    public <R> R accept(LiteralVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
