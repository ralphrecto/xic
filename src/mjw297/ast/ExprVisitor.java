package mjw297.ast;

public interface ExprVisitor<R> {
    public R visit(Literal l);
    public R visit(BinOp o);
    public R visit(UnOp o);
    public R visit(Index i);
    public R visit(Length l);
    public R visit(ParenthesizedExpr e);
    public R visit(Call c);
}
