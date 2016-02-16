package mjw297.ast;

import java.util.List;

public final class Program {
    public final List<Use> uses;
    public final List<Callable> fs;
    public Program(List<Use> uses, List<Callable> fs) {
        this.uses = uses;
        this.fs = fs;
    }
}
