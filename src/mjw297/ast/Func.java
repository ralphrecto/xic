package mjw297.ast;

import java.util.List;

public final class Func implements Callable {
    public final Id name;
    public final List<AnnotatedVar> args;
    public final Type returnType;
    public final List<Stmt> body;
    public final List<Expr> returns;
    public Func(Id name, List<AnnotatedVar> args, Type returnType,
                List<Stmt> body, List<Expr> returns) {
        this.name = name;
        this.args = args;
        this.returnType = returnType;
        this.body = body;
        this.returns = returns;
    }
    public <R> R accept(CallableVisitor<R> v) {
        return v.visit(this);
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
