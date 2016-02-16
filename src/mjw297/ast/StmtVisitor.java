package mjw297.ast;

public interface StmtVisitor<R> {
    public R visit(Decl d);
    public R visit(DeclAsgn d);
    public R visit(If i);
    public R visit(IfElse i);
    public R visit(While w);
    public R visit(Call c);
}
