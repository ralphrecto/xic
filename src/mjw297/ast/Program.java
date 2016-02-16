package mjw297.ast;

import java.util.List;

public final class Program implements Node {
    public final List<Use> uses;
    public final List<Callable> fs;
    public Program(List<Use> uses, List<Callable> fs) {
        this.uses = uses;
        this.fs = fs;
    }
    public <R> R accept(NodeVisitor<R> v) {
        return v.visit(this);
    }
}
