package mjw297;

import java.util.List;
import java.util.Optional;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.ToString;

public interface Ast {
    ////////////////////////////////////////////////////////////////////////////
    // Interfaces
    ////////////////////////////////////////////////////////////////////////////
    public interface Node {
        public <R> R accept(NodeVisitor<R> v);
    }
    public interface AnnotatedVar extends Var  {}
    public interface Callable     extends Node {}
    public interface Expr         extends Node {}
    public interface Literal      extends Expr {}
    public interface Stmt         extends Node {}
    public interface Type         extends Node {}
    public interface Var          extends Node {}

    ////////////////////////////////////////////////////////////////////////////
    // Visitors
    ////////////////////////////////////////////////////////////////////////////
    public interface NodeVisitor<R> {
        // AnnotatedVar
        public R visit(AnnotatedId i);
        public R visit(AnnotatedUnderscore u);

        // Callable
        public R visit(Func f);
        public R visit(Proc p);

        // Expr
        public R visit(Id i);
        public R visit(BinOp o);
        public R visit(UnOp o);
        public R visit(Index i);
        public R visit(Length l);
        public R visit(ParenthesizedExpr e);

        // Literal
        public R visit(NumLiteral n);
        public R visit(BoolLiteral b);
        public R visit(StringLiteral s);
        public R visit(CharLiteral c);
        public R visit(ArrayLiteral a);

        // Program
        public R visit(Program p);

        // Stmt
        public R visit(Decl d);
        public R visit(DeclAsgn d);
        public R visit(Asgn a);
        public R visit(If i);
        public R visit(IfElse i);
        public R visit(While w);

        // Type
        public R visit(Int l);
        public R visit(Bool o);
        public R visit(Array o);

        // Use
        public R visit(Use u);

        // Var
        public R visit(Underscore u);

        // Expr, Stmt
        public R visit(Call c);
    }

    public interface AnnotatedVarVisitor<R> {
        public R visit(AnnotatedId i);
        public R visit(AnnotatedUnderscore u);
    }

    public interface CallableVisitor<R> {
        public R visit(Func f);
        public R visit(Proc p);
    }

    public interface ExprVisitor<R> {
        public R visit(Id i);
        public R visit(Literal l);
        public R visit(BinOp o);
        public R visit(UnOp o);
        public R visit(Index i);
        public R visit(Length l);
        public R visit(ParenthesizedExpr e);
        public R visit(Call c);
    }

    public interface LiteralVisitor<R> {
        public R visit(NumLiteral n);
        public R visit(BoolLiteral b);
        public R visit(StringLiteral s);
        public R visit(CharLiteral c);
        public R visit(ArrayLiteral a);
    }

    public interface StmtVisitor<R> {
        public R visit(Decl d);
        public R visit(DeclAsgn d);
        public R visit(Asgn a);
        public R visit(If i);
        public R visit(IfElse i);
        public R visit(While w);
        public R visit(Call c);
    }

    public interface TypeVisitor<R> {
        public R visit(Int l);
        public R visit(Bool o);
        public R visit(Array o);
    }

    public interface VarVisitor<R> {
        public R visit(AnnotatedVar v);
        public R visit(Underscore u);
    }

    ////////////////////////////////////////////////////////////////////////////
    // AnnotatedVar
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class AnnotatedId implements AnnotatedVar {
        public final Id x;
        public final Type t;
        public <R> R accept(AnnotatedVarVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class AnnotatedUnderscore implements AnnotatedVar {
        public final Underscore u;
        public final Type t;
        public <R> R accept(AnnotatedVarVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Callable
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Func implements Callable {
        public final Id name;
        public final List<AnnotatedVar> args;
        public final Type returnType;
        public final List<Stmt> body;
        public final List<Expr> returns;
        public <R> R accept(CallableVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Proc implements Callable {
        public final Id name;
        public final List<AnnotatedVar> args;
        public final List<Stmt> body;
        public <R> R accept(CallableVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Expr
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Id implements Expr {
        public final String x;
        public <R> R accept(ExprVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    public enum BinOpCode {
        MINUS,    // -
        STAR,     // *
        HIGHMULT, // *>>
        DIV,      // /
        MOD,      // %
        PLUS,     // +
        LT,       // <
        LTE,      // <=
        GTE,      // >=
        GT,       // >
        EQEQ,     // ==
        NEQ,      // !=
        AMP,      // &
        BAR       // |
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class BinOp implements Expr {
        public final BinOpCode c;
        public final Expr lhs;
        public final Expr rhs;
        public <R> R accept(ExprVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    public enum UnOpCode {
        UMINUS, // -
        BANG    // !
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class UnOp implements Expr {
        public final UnOpCode c;
        public final Expr e;
        public <R> R accept(ExprVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Index implements Expr {
        public final Expr e;
        public final Expr index;
        public <R> R accept(ExprVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Length implements Expr {
        public final Expr e;
        public <R> R accept(ExprVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class ParenthesizedExpr implements Expr {
        public final Expr e;
        public <R> R accept(ExprVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Literal
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class NumLiteral implements Literal {
        public final long x;
        public <R> R accept(LiteralVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class BoolLiteral implements Literal {
        public final boolean b;
        public <R> R accept(LiteralVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class StringLiteral implements Literal {
        public final String s;
        public <R> R accept(LiteralVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class CharLiteral implements Literal {
        public final char c;
        public <R> R accept(LiteralVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class ArrayLiteral implements Literal {
        public final List<Expr> xs;
        public <R> R accept(LiteralVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Program
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Program implements Node {
        public final List<Use> uses;
        public final List<Callable> fs;
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Stmt
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Decl implements Stmt {
        public final List<Var> vs;
        public <R> R accept(StmtVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class DeclAsgn implements Stmt {
        public final List<Var> vs;
        public final Expr e;
        public <R> R accept(StmtVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Asgn implements Stmt {
        // TODO: not sure
        public <R> R accept(StmtVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class If implements Stmt {
        public final Expr b;
        public final Stmt body;
        public <R> R accept(StmtVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class IfElse implements Stmt {
        public final Expr b;
        public final Stmt thenBody;
        public final Stmt elseBody;
        public <R> R accept(StmtVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class While implements Stmt {
        public final Expr b;
        public final Stmt body;
        public <R> R accept(StmtVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Type
    ////////////////////////////////////////////////////////////////////////////
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Int implements Type {
        public <R> R accept(TypeVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Bool implements Type {
        public <R> R accept(TypeVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Array implements Type {
        public final Type t;
        public final Optional<Expr> size;
        public <R> R accept(TypeVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Use
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Use implements Node {
        public final Id x;
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Var
    ////////////////////////////////////////////////////////////////////////////
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Underscore implements Var {
        public <R> R accept(VarVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Expr, Stmt
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Call implements Expr, Stmt {
        public final Id f;
        public final List<Expr> args;
        public <R> R accept(ExprVisitor<R> v) { return v.visit(this); }
        public <R> R accept(StmtVisitor<R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Example
    ////////////////////////////////////////////////////////////////////////////
    static class UglyPrinter implements NodeVisitor<String> {
        public String visit(AnnotatedId i)         { return "TODO"; }
        public String visit(AnnotatedUnderscore u) { return "TODO"; }
        public String visit(Func f)                { return "TODO"; }
        public String visit(Proc p)                { return "TODO"; }
        public String visit(Id i)                  { return "TODO"; }
        public String visit(BinOp o)               { return "TODO"; }
        public String visit(UnOp o)                { return "TODO"; }
        public String visit(Index i)               { return "TODO"; }
        public String visit(Length l)              { return "TODO"; }
        public String visit(ParenthesizedExpr e)   { return "TODO"; }
        public String visit(NumLiteral n)          { return "TODO"; }
        public String visit(BoolLiteral b)         { return "TODO"; }
        public String visit(StringLiteral s)       { return "TODO"; }
        public String visit(CharLiteral c)         { return "TODO"; }
        public String visit(ArrayLiteral a)        { return "TODO"; }
        public String visit(Program p)             { return "TODO"; }
        public String visit(Decl d)                { return "TODO"; }
        public String visit(DeclAsgn d)            { return "TODO"; }
        public String visit(Asgn a)                { return "TODO"; }
        public String visit(If i)                  { return "TODO"; }
        public String visit(IfElse i)              { return "TODO"; }
        public String visit(While w)               { return "TODO"; }
        public String visit(Int l)                 { return "TODO"; }
        public String visit(Bool o)                { return "TODO"; }
        public String visit(Array o)               { return "TODO"; }
        public String visit(Use u)                 { return "TODO"; }
        public String visit(Underscore u)          { return "TODO"; }
        public String visit(Call c)                { return "TODO"; }
    }

    public static void main(String[] args) {
        AnnotatedId i1 = new AnnotatedId(new Id("x"), new Int());
        AnnotatedId i2 = new AnnotatedId(new Id("x"), new Int());
        UglyPrinter p = new UglyPrinter();
        System.out.println(i1.accept(p));
        System.out.println(i2.accept(p));
        System.out.println("i1 == i2 is " + i1.equals(i2));
        System.out.println(i1.toString());
    }
}
