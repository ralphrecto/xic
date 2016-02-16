package mjw297.ast;

public interface NodeVisitor<R> {
    public R visit(AnnotatedId i);
    public R visit(AnnotatedUnderscore u);
    public R visit(Array a);
    public R visit(ArrayLiteral a);
    public R visit(Asgn a);
    public R visit(BinOp b);
    public R visit(Bool b);
    public R visit(BoolLiteral b);
    public R visit(Call c);
    public R visit(CharLiteral c);
    public R visit(DeclAsgn d);
    public R visit(Decl d);
    public R visit(Func f);
    public R visit(Id i);
    public R visit(IfElse i);
    public R visit(If i);
    public R visit(Index i);
    public R visit(Int i);
    public R visit(Length l);
    public R visit(NumLiteral n);
    public R visit(ParenthesizedExpr p);
    public R visit(Proc p);
    public R visit(Program p);
    public R visit(StringLiteral s);
    public R visit(Underscore u);
    public R visit(UnOp u);
    public R visit(Use u);
    public R visit(While w);
}
