package mjw297.ast;

import java.util.List;

public final class Proc implements Callable {
    public final Id name;
    public final List<AnnotatedVar> args;
    public final List<Stmt> body;
    public Proc(Id name, List<AnnotatedVar> args, List<Stmt> body) {
        this.name = name;
        this.args = args;
        this.body = body;
    }
    public <R> R accept(CallableVisitor<R> v) {
        return v.visit(this);
    }
}
