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
    public interface AnnotatedVar<A> extends Var<A>  {
        public <R> R accept(AnnotatedVarVisitor<A, R> v);
    }
    public interface Callable<A> extends Node<A> {
        public <R> R accept(CallableVisitor<A, R> v);
    }
    public interface Expr<A> extends Node<A> {
        public <R> R accept(ExprVisitor<A, R> v);
    }
    public interface Literal<A> extends Expr<A> {
        public <R> R accept(LiteralVisitor<A, R> v);
    }
    public interface Stmt<A> extends Node<A> {
        public <R> R accept(StmtVisitor<A, R> v);
    }
    public interface Type<A> extends Node<A> {
        public <R> R accept(TypeVisitor<A, R> v);
    }
    public interface Var<A> extends Node<A> {
        public <R> R accept(VarVisitor<A, R> v);
    }
    public interface CallableDecl<A> extends Node<A> { }

    public interface XiFile<A> extends Node<A> { }

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
        public R visit(FuncCall<A> c);

        // Literal
        public R visit(NumLiteral<A> n);
        public R visit(BoolLiteral<A> b);
        public R visit(StringLiteral<A> s);
        public R visit(CharLiteral<A> c);
        public R visit(ArrayLiteral<A> a);

        // Program
        public R visit(Program<A> p);

        // Interfaces
        public R visit(FuncDecl<A> p);
        public R visit(ProcDecl<A> p);
        public R visit(Interface<A> p);
        public R visit(FullProgram<A> p);

        // Stmt
        public R visit(Decl<A> d);
        public R visit(DeclAsgn<A> d);
        public R visit(Asgn<A> a);
        public R visit(UnderscoreAsgn<A> a);
        public R visit(Block<A> b);
        public R visit(If<A> i);
        public R visit(IfElse<A> i);
        public R visit(While<A> w);
        public R visit(ProcCall<A> c);

        // Type
        public R visit(Int<A> l);
        public R visit(Bool<A> o);
        public R visit(Array<A> o);

        // Use
        public R visit(Use<A> u);

        // Var
        public R visit(Underscore<A> u);
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
        public R visit(BinOp<A> o);
        public R visit(UnOp<A> o);
        public R visit(Index<A> i);
        public R visit(Length<A> l);
        public R visit(FuncCall<A> c);

        public R visit(NumLiteral<A> n);
        public R visit(BoolLiteral<A> b);
        public R visit(StringLiteral<A> s);
        public R visit(CharLiteral<A> c);
        public R visit(ArrayLiteral<A> a);
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
        public R visit(UnderscoreAsgn<A> a);
        public R visit(Block<A> b);
        public R visit(If<A> i);
        public R visit(IfElse<A> i);
        public R visit(While<A> w);
        public R visit(ProcCall<A> c);
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
        public <R> R accept(VarVisitor<A, R> v) { return v.visit(this); }
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
        public <R> R accept(VarVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Callable
    ////////////////////////////////////////////////////////////////////////////

    /* Implementation note: the bodies of Procs and Funcs are both a single stmt.
     * The grammar disallows for this stmt to be anything but a block. This is
     * done to make folding over ASTs easier. */

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Func<A> implements Callable<A> {
        public final A a;
        public final Id<A> name;
        public final List<AnnotatedVar<A>> args;
        public final List<Type<A>> returnType;
        public final Stmt<A> body;
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
        public final Stmt<A> body;
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
        MINUS(Sym.MINUS),       // -
        STAR(Sym.STAR),         // *
        HIGHMULT(Sym.HIGHMULT), // *>>
        DIV(Sym.DIV),           // /
        MOD(Sym.MOD),           // %
        PLUS(Sym.PLUS),         // +
        LT(Sym.LT),             // <
        LTE(Sym.LTE),           // <=
        GTE(Sym.GTE),           // >=
        GT(Sym.GT),             // >
        EQEQ(Sym.EQEQ),         // ==
        NEQ(Sym.NEQ),           // !=
        AMP(Sym.AMP),           // &
        BAR(Sym.BAR);           // |

        public final int code;
        BinOpCode(int code) {
             this.code = code;
        }
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
        UMINUS(Sym.MINUS), // -
        BANG(Sym.BANG);    // !

        public final int code;
        UnOpCode(int code) {
             this.code = code;
        }
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
        public final Expr<A> index;
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
    public final class FuncCall<A> implements Expr<A> {
        public final A a;
        public final Id<A> f;
        public final List<Expr<A>> args;
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
        public <R> R accept(ExprVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class BoolLiteral<A> implements Literal<A> {
        public final A a;
        public final boolean b;
        public <R> R accept(LiteralVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(ExprVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class StringLiteral<A> implements Literal<A> {
        public final A a;
        public final String s;
        public <R> R accept(LiteralVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(ExprVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class CharLiteral<A> implements Literal<A> {
        public final A a;
        public final char c;
        public <R> R accept(LiteralVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(ExprVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class ArrayLiteral<A> implements Literal<A> {
        public final A a;
        public final List<Expr<A>> xs;
        public <R> R accept(LiteralVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(ExprVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Program
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Program<A> implements XiFile<A> {
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
        public final Expr<A> lhs;
        public final Expr<A> rhs;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class UnderscoreAsgn<A> implements Stmt<A> {
        public final A a;
        public final Var<A> lhs;
        public final Expr<A> rhs;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Block<A> implements Stmt<A> {
        public final A a;
        public final List<Stmt<A>> ss;
        public final Optional<List<Expr<A>>> ret;
        public final A ret_a;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class If<A> implements Stmt<A> {
        public final A a;
        public final Expr<A> b;
        public final Stmt<A> body;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class IfElse<A> implements Stmt<A> {
        public final A a;
        public final Expr<A> b;
        public final Stmt<A> thenBody;
        public final Stmt<A> elseBody;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class While<A> implements Stmt<A> {
        public final A a;
        public final Expr<A> b;
        public final Stmt<A> body;
        public <R> R accept(StmtVisitor<A, R> v) { return v.visit(this); }
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class ProcCall<A> implements Stmt<A> {
        public final A a;
        public final Id<A> f;
        public final List<Expr<A>> args;
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
        public final Optional<Expr<A>> size;
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
    // Interface Declarations
    ////////////////////////////////////////////////////////////////////////////
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class FuncDecl<A> implements CallableDecl<A> {
        public final A a;
        public final Id<A> name;
        public final List<AnnotatedVar<A>> args;
        public final List<Type<A>> returnType;
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class ProcDecl<A> implements CallableDecl<A> {
        public final A a;
        public final Id<A> name;
        public final List<AnnotatedVar<A>> args;
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class Interface<A> implements XiFile<A> {
        public final A a;
        public final List<CallableDecl<A>> fs;
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }

    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public final class FullProgram<A> implements Node<A> {
        public final A a;
        public final String progname;
        public final Program<A> prog;
        public final List<Interface<A>> inters;
        public <R> R accept(NodeVisitor<A, R> v) { return v.visit(this); }
    }
}
