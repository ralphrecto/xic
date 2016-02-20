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
    public interface Node<A> {
        public <R> R accept(NodeVisitor<A, R> v);
    }
    public interface AnnotatedVar<A> extends Var<A>  {}
    public interface Callable<A>     extends Node<A> {}
    public interface Expr<A>         extends Node<A> {}
    public interface Literal<A>      extends Expr<A> {}
    public interface Stmt<A>         extends Node<A> {}
    public interface Type<A>         extends Node<A> {}
    public interface Var<A>          extends Node<A> {}

    ////////////////////////////////////////////////////////////////////////////
    // Visitors
    ////////////////////////////////////////////////////////////////////////////
    public interface NodeVisitor<A, R> {
        // AnnotatedVar
        public R visit(AnnotatedId<A> i);
        public R visit(AnnotatedUnderscore<A> u);

        // Callable
        public R visit(Func<A> f);
        public R visit(Proc<A> p);

        // Expr
        public R visit(Id<A> i);
        public R visit(BinOp<A> o);
        public R visit(UnOp<A> o);
        public R visit(Index<A> i);
        public R visit(Length<A> l);
        public R visit(ParenthesizedExpr<A> e);

        // Literal
        public R visit(NumLiteral<A> n);
        public R visit(BoolLiteral<A> b);
        public R visit(StringLiteral<A> s);
        public R visit(CharLiteral<A> c);
        public R visit(ArrayLiteral<A> a);

        // Program
        public R visit(Program<A> p);

        // Stmt
        public R visit(Decl<A> d);
        public R visit(DeclAsgn<A> d);
        public R visit(Asgn<A> a);
        public R visit(AsgnArrayIndex<A> a);
        public R visit(If<A> i);
        public R visit(IfElse<A> i);
        public R visit(While<A> w);

        // Type
        public R visit(Int<A> l);
        public R visit(Bool<A> o);
        public R visit(Array<A> o);

        // Use
        public R visit(Use<A> u);

        // Var
        public R visit(Underscore<A> u);

        // Expr, Stmt
        public R visit(Call<A> c);
    }

    public interface AnnotatedVarVisitor<A, R> {
        public R visit(AnnotatedId<A> i);
        public R visit(AnnotatedUnderscore<A> u);
    }

    public interface CallableVisitor<A, R> {
        public R visit(Func<A> f);
        public R visit(Proc<A> p);
    }

    public interface ExprVisitor<A, R> {
        public R visit(Id<A> i);
        public R visit(Literal<A> l);
        public R visit(BinOp<A> o);
        public R visit(UnOp<A> o);
        public R visit(Index<A> i);
        public R visit(Length<A> l);
        public R visit(ParenthesizedExpr<A> e);
        public R visit(Call<A> c);
    }

    public interface LiteralVisitor<A, R> {
        public R visit(NumLiteral<A> n);
        public R visit(BoolLiteral<A> b);
        public R visit(StringLiteral<A> s);
        public R visit(CharLiteral<A> c);
        public R visit(ArrayLiteral<A> a);
    }

    public interface StmtVisitor<A, R> {
        public R visit(Decl<A> d);
        public R visit(DeclAsgn<A> d);
        public R visit(Asgn<A> a);
        public R visit(AsgnArrayIndex<A> a);
        public R visit(If<A> i);
        public R visit(IfElse<A> i);
        public R visit(While<A> w);
        public R visit(Call<A> c);
    }

    public interface TypeVisitor<A, R> {
        public R visit(Int<A> l);
        public R visit(Bool<A> o);
        public R visit(Array<A> o);
    }

    public interface VarVisitor<A, R> {
        public R visit(AnnotatedVar<A> v);
        public R visit(Underscore<A> u);
    }

    ////////////////////////////////////////////////////////////////////////////
    // AnnotatedVar
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class AnnotatedId<A> implements AnnotatedVar<A> {
        public final A a;
        public final Id<A> x;
        public final Type<A> t;
        public <R> R accept(AnnotatedVarVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class AnnotatedUnderscore<A> implements AnnotatedVar<A> {
        public final A a;
        public final Underscore<A> u;
        public final Type<A> t;
        public <R> R accept(AnnotatedVarVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Callable
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Func<A> implements Callable<A> {
        public final A a;
        public final Id<A> name;
        public final List<AnnotatedVar<A>> args;
        public final List<Type<A>> returnType;
        public final List<Stmt<A>> body;
        public final List<Expr<A>> returns;
        public <R> R accept(CallableVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Proc<A> implements Callable<A> {
        public final A a;
        public final Id<A> name;
        public final List<AnnotatedVar<A>> args;
        public final List<Stmt<A>> body;
        public <R> R accept(CallableVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Expr
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Id<A> implements Expr<A> {
        public final A a;
        public final String x;
        public <R> R accept(ExprVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
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

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class BinOp<A> implements Expr<A> {
        public final A a;
        public final BinOpCode c;
        public final Expr<A> lhs;
        public final Expr<A> rhs;
        public <R> R accept(ExprVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    public enum UnOpCode {
        UMINUS, // -
        BANG    // !
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class UnOp<A> implements Expr<A> {
        public final A a;
        public final UnOpCode c;
        public final Expr<A> e;
        public <R> R accept(ExprVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Index<A> implements Expr<A> {
        public final A a;
        public final Expr<A> e;
        public final List<Expr<A>> index;
        public <R> R accept(ExprVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Length<A> implements Expr<A> {
        public final A a;
        public final Expr<A> e;
        public <R> R accept(ExprVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class ParenthesizedExpr<A> implements Expr<A> {
        public final A a;
        public final Expr<A> e;
        public <R> R accept(ExprVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Literal
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class NumLiteral<A> implements Literal<A> {
        public final A a;
        public final long x;
        public <R> R accept(LiteralVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class BoolLiteral<A> implements Literal<A> {
        public final A a;
        public final boolean b;
        public <R> R accept(LiteralVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class StringLiteral<A> implements Literal<A> {
        public final A a;
        public final String s;
        public <R> R accept(LiteralVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class CharLiteral<A> implements Literal<A> {
        public final A a;
        public final char c;
        public <R> R accept(LiteralVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class ArrayLiteral<A> implements Literal<A> {
        public final A a;
        public final List<Expr<A>> xs;
        public <R> R accept(LiteralVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Program
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Program<A> implements Node<A> {
        public final A a;
        public final List<Use<A>> uses;
        public final List<Callable<A>> fs;
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Stmt
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Decl<A> implements Stmt<A> {
        public final A a;
        public final List<Var<A>> vs;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class DeclAsgn<A> implements Stmt<A> {
        public final A a;
        public final List<Var<A>> vs;
        public final Expr<A> e;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Asgn<A> implements Stmt<A> {
        public final A a;
        public final Id<A> id;
        public final Expr<A> expr;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class AsgnArrayIndex<A> implements Stmt<A> {
        public final A a;
		public final Id<A> id;
		public final List<Expr<A>> index;
        public final Expr<A> expr;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class If<A> implements Stmt<A> {
        public final A a;
        public final Expr<A> b;
        public final List<Stmt<A>> body;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class IfElse<A> implements Stmt<A> {
        public final A a;
        public final Expr<A> b;
        public final List<Stmt<A>> thenBody;
        public final List<Stmt<A>> elseBody;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class While<A> implements Stmt<A> {
        public final A a;
        public final Expr<A> b;
        public final List<Stmt<A>> body;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Type
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Int<A> implements Type<A> {
        public final A a;
        public <R> R accept(TypeVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Bool<A> implements Type<A> {
        public final A a;
        public <R> R accept(TypeVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Array<A> implements Type<A> {
        public final A a;
        public final Type<A> t;
        public final Optional<List<Expr<A>>> size;
        public <R> R accept(TypeVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Use
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Use<A> implements Node<A> {
        public final A a;
        public final Id<A> x;
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Var
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Underscore<A> implements Var<A> {
        public final A a;
        public <R> R accept(VarVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Expr, Stmt
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Call<A> implements Expr<A>, Stmt<A> {
        public final A a;
        public final Id<A> f;
        public final List<Expr<A>> args;
        public <R> R accept(ExprVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Example
    ////////////////////////////////////////////////////////////////////////////
    static class UglyPrinter implements NodeVisitor<Position, String> {
        public String visit(AnnotatedId<Position> i)         { return i.toString(); }
        public String visit(AnnotatedUnderscore<Position> u) { return u.toString(); }
        public String visit(Func<Position> f)                { return f.toString(); }
        public String visit(Proc<Position> p)                { return p.toString(); }
        public String visit(Id<Position> i)                  { return i.toString(); }
        public String visit(BinOp<Position> o)               { return o.toString(); }
        public String visit(UnOp<Position> o)                { return o.toString(); }
        public String visit(Index<Position> i)               { return i.toString(); }
        public String visit(Length<Position> l)              { return l.toString(); }
        public String visit(ParenthesizedExpr<Position> e)   { return e.toString(); }
        public String visit(NumLiteral<Position> n)          { return n.toString(); }
        public String visit(BoolLiteral<Position> b)         { return b.toString(); }
        public String visit(StringLiteral<Position> s)       { return s.toString(); }
        public String visit(CharLiteral<Position> c)         { return c.toString(); }
        public String visit(ArrayLiteral<Position> a)        { return a.toString(); }
        public String visit(Program<Position> p)             { return p.toString(); }
        public String visit(Decl<Position> d)                { return d.toString(); }
        public String visit(DeclAsgn<Position> d)            { return d.toString(); }
        public String visit(Asgn<Position> a)                { return a.toString(); }
        public String visit(AsgnArrayIndex<Position> a)      { return a.toString(); }
        public String visit(If<Position> i)                  { return i.toString(); }
        public String visit(IfElse<Position> i)              { return i.toString(); }
        public String visit(While<Position> w)               { return w.toString(); }
        public String visit(Int<Position> l)                 { return l.toString(); }
        public String visit(Bool<Position> o)                { return o.toString(); }
        public String visit(Array<Position> o)               { return o.toString(); }
        public String visit(Use<Position> u)                 { return u.toString(); }
        public String visit(Underscore<Position> u)          { return u.toString(); }
        public String visit(Call<Position> c)                { return c.toString(); }
    }

    public static void main(String[] args) {
        Position p1 = new Position(1, 1);
        Position p2 = new Position(2, 2);
        AnnotatedId<Position> i1 = AnnotatedId.of(p1, Id.of(p1, "x"), Int.of(p2));
        AnnotatedId<Position> i2 = AnnotatedId.of(p1, Id.of(p1, "x"), Int.of(p2));
        AnnotatedId<Position> i3 = AnnotatedId.of(p2, Id.of(p2, "x"), Int.of(p1));
        AnnotatedId<Position> i4 = AnnotatedId.of(p1, Id.of(p1, "y"), Int.of(p2));
        UglyPrinter p = new UglyPrinter();
        System.out.println(i1.accept(p));
        System.out.println(i2.accept(p));
        System.out.println(i3.accept(p));
        System.out.println(i4.accept(p));

        System.out.println(i1.toString());
        System.out.println(i2.toString());
        System.out.println(i3.toString());
        System.out.println(i4.toString());

        System.out.println("i1 == i1 is " + i1.equals(i1));
        System.out.println("i1 == i2 is " + i1.equals(i2));
        System.out.println("i1 == i3 is " + i1.equals(i3));
        System.out.println("i1 == i4 is " + i1.equals(i4));

        System.out.println("i2 == i1 is " + i2.equals(i1));
        System.out.println("i2 == i2 is " + i2.equals(i2));
        System.out.println("i2 == i3 is " + i2.equals(i3));
        System.out.println("i2 == i4 is " + i2.equals(i4));

        System.out.println("i3 == i1 is " + i3.equals(i1));
        System.out.println("i3 == i2 is " + i3.equals(i2));
        System.out.println("i3 == i3 is " + i3.equals(i3));
        System.out.println("i3 == i4 is " + i3.equals(i4));

        System.out.println("i4 == i1 is " + i4.equals(i1));
        System.out.println("i4 == i2 is " + i4.equals(i2));
        System.out.println("i4 == i3 is " + i4.equals(i3));
        System.out.println("i4 == i4 is " + i4.equals(i4));
    }
}
