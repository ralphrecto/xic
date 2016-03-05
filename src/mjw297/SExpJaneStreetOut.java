package mjw297;

import edu.cornell.cs.cs4120.util.CodeWriterSExpPrinter;
import edu.cornell.cs.cs4120.util.SExpPrinter;

import mjw297.Ast.*;

import java.io.OutputStream;
import java.util.List;

class SExpJaneStreetOut implements Ast.NodeVisitor<Position, Void> {
    SExpPrinter printer;
    SExpJaneStreetOut(OutputStream o) {
        this.printer = new CodeWriterSExpPrinter(o);
    }

    class AnnotatedVarPos implements Ast.AnnotatedVarVisitor<Position, Position> {
        public Position visit(AnnotatedId<Position> i) {
            return i.a;
        }
        public Position visit(AnnotatedUnderscore<Position> u) {
            return u.a;
        }
    }

    class SExpVar implements Ast.VarVisitor<Position, Void> {
        SExpJaneStreetOut parent;
        SExpPrinter printer;

        SExpVar(SExpJaneStreetOut parent) {
            this.parent = parent;
            this.printer = parent.printer;
        }

        @Override
        public Void visit(AnnotatedVar<Position> v) {
            printer.startList();
            posPrinter(v.accept(new AnnotatedVarPos()));
            printer.startList();
            printer.printAtom("AVar");
            v.accept(this.parent);
            printer.endList();
            printer.endList();

            return null;
        }

        @Override
        public Void visit(Underscore<Position> u) {
            printer.startList();
            posPrinter(u.a);
            printer.printAtom("Underscore");
            printer.endList();

            return null;
        }
    }

    private void posPrinter(Position p) {
        this.printer.startList();
        this.printer.printAtom(((Integer) p.row).toString());
        this.printer.printAtom(((Integer) p.col).toString());
        this.printer.endList();
    }

    private void rawIdPrinter(Ast.Id<Position> i) {
        printer.startList();
        posPrinter(i.a);
        printer.printAtom(i.x);
        printer.endList();
    }

    public Void visit(Ast.AnnotatedId<Position> i) {
        printer.startList();
        posPrinter(i.a);

        printer.startList();
        printer.printAtom("AId");

        rawIdPrinter(i.x);
        i.t.accept(this);

        printer.endList();
        printer.endList();

        return null;
    }

    public Void visit(Ast.AnnotatedUnderscore<Position> u) {
        printer.startList();
        posPrinter(u.a);
        printer.startList();
        printer.printAtom("AUnderscore");
        u.t.accept(this);
        printer.endList();
        printer.endList();

        return null;
    }

    public Void visit(Ast.Func<Position> f) {
        /* function name */
        printer.startList();
        posPrinter(f.a);

        printer.startList();
        printer.printAtom("Func");
        rawIdPrinter(f.name);

        /* proc parameters */
        printer.startList();
        f.args.forEach(a -> a.accept(this));
        printer.endList();

        /* return types */
        printer.startList();
        f.returnType.forEach(t -> t.accept(this));
        printer.endList();

        f.body.accept(this);

        printer.endList();
        printer.endList();

        return null;
    }

    public Void visit(Ast.Proc<Position> p) {
        /* function name */
        printer.startList();
        posPrinter(p.a);

        printer.startList();
        printer.printAtom("Proc");

        rawIdPrinter(p.name);

        /* proc parameters */
        printer.startList();
        p.args.forEach(av -> av.accept(this));
        printer.endList();

        /* block */
        p.body.accept(this);

        printer.endList();
        printer.endList();

        return null;
    }

    public Void visit(Ast.Id<Position> i) {
        printer.startList();
        posPrinter(i.a);
        printer.startList();
        printer.printAtom("Id");
        rawIdPrinter(i);
        printer.endList();
        printer.endList();

        return null;
    }

    public Void visit(Ast.BinOp<Position> o) {
        printer.startList();
        posPrinter(o.a);
        printer.startList();
        printer.printAtom("BinOp");
        o.lhs.accept(this);
        printer.printAtom(o.c.name());
        o.rhs.accept(this);
        printer.endList();
        printer.endList();

        return null;
    }

    public Void visit(Ast.UnOp<Position> o) {
        printer.startList();
        posPrinter(o.a);
        printer.startList();
        printer.printAtom("UnOp");
        printer.printAtom(o.c.name());
        o.e.accept(this);
        printer.endList();
        printer.endList();

        return null;
    }

    public Void visit(Ast.Index<Position> i) {
        printer.startList();
        posPrinter(i.a);
        printer.startList();
        printer.printAtom("Index");
        i.e.accept(this);
        i.index.accept(this);
        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.Length<Position> l) {
        printer.startList();
        posPrinter(l.a);
        printer.startList();
        printer.printAtom("Length");
        l.e.accept(this);
        printer.endList();
        printer.endList();

        return null;
    }

    public Void visit(Ast.NumLiteral<Position> n) {
        printer.startList();
        posPrinter(n.a);
        printer.startList();
        printer.printAtom("Int");
        printer.printAtom(((Long) n.x).toString());
        printer.endList();
        printer.endList();

        return null;
    }

    public Void visit(Ast.BoolLiteral<Position> b) {
        printer.startList();
        posPrinter(b.a);
        printer.startList();
        printer.printAtom("Bool");
        printer.printAtom(((Boolean) b.b).toString());
        printer.endList();
        printer.endList();

        return null;
    }

    public Void visit(Ast.StringLiteral<Position> s) {
        printer.startList();
        posPrinter(s.a);
        printer.startList();
        printer.printAtom("String");
        printer.printAtom(
                String.format("\"%s\"", SymUtil.prettyPrintString(s.s))
        );
        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.CharLiteral<Position> c) {
        printer.startList();
        posPrinter(c.a);
        printer.startList();
        printer.printAtom("Char");
        printer.printAtom(
                String.format("%s", SymUtil.prettyPrintChar(c.c))
        );
        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.ArrayLiteral<Position> a) {
        printer.startList();
        posPrinter(a.a);
        printer.startList();
        printer.printAtom("Array");

        /* Actual list contents */
        printer.startList();
        a.xs.forEach(e -> e.accept(this));
        printer.endList();

        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.Program<Position> p) {
        printer.startList();
        posPrinter(p.a);

        printer.startList();
        printer.printAtom("Prog");

        printer.startList();
        p.uses.forEach((u -> u.accept(this)));
        printer.endList();

        printer.startList();
        p.fs.forEach(f -> f.accept(this));
        printer.endList();

        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.Decl<Position> d) {
        printer.startList();
        posPrinter(d.a);
        printer.startList();
        printer.printAtom("Decl");

        printer.startList();
        SExpVar varVisitor = new SExpVar(this);
        d.vs.forEach(v -> v.accept(varVisitor));
        printer.endList();

        printer.endList();
        printer.endList();

        return null;
    }

    public Void visit(Ast.DeclAsgn<Position> d) {
        printer.startList();
        posPrinter(d.a);
        printer.startList();
        printer.printAtom("DeclAsgn");

        printer.startList();
        SExpVar varVisitor = new SExpVar(this);
        d.vs.forEach(v -> v.accept(varVisitor));
        printer.endList();

        d.e.accept(this);

        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.Asgn<Position> a) {
        printer.startList();
        posPrinter(a.a);
        printer.startList();
        printer.printAtom("Asgn");

        a.lhs.accept(this);
        a.rhs.accept(this);

        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.UnderscoreAsgn<Position> a) {
        return null;
    }

    @Override
    public Void visit(Ast.Block<Position> b) {
        printer.startList();
        posPrinter(b.a);

        printer.startList();
        printer.printAtom("Block");

        /* stmt list */
        printer.startList();
        b.ss.forEach(s -> s.accept(this));
        if (b.ret.isPresent()) {
            printer.startList();
            posPrinter(b.ret_a);

            printer.startList();
            printer.printAtom("Return");

            printer.startList();
            b.ret.get().forEach(e -> e.accept(this));
            printer.endList();

            printer.endList();

            printer.endList();
        }
        printer.endList();

        printer.endList();

        printer.endList();
        return null;
    }

    public Void visit(Ast.If<Position> i) {
        printer.startList();
        posPrinter(i.a);
        printer.startList();
        printer.printAtom("If");

        i.b.accept(this);
        i.body.accept(this);

        printer.endList();
        printer.endList();

        return null;
    }

    public Void visit(Ast.IfElse<Position> i) {
        printer.startList();
        posPrinter(i.a);
        printer.startList();
        printer.printAtom("IfElse");

        i.b.accept(this);
        i.thenBody.accept(this);
        i.elseBody.accept(this);

        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.While<Position> w) {
        printer.startList();
        posPrinter(w.a);
        printer.startList();
        printer.printAtom("While");

        /* predicate */
        w.b.accept(this);

        /* body */
        w.body.accept(this);

        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.Int<Position> l) {
        printer.startList();
        posPrinter(l.a);
        printer.printAtom("TInt");
        printer.endList();
        return null;
    }

    public Void visit(Ast.Bool<Position> o) {
        printer.startList();
        posPrinter(o.a);
        printer.printAtom("TBool");
        printer.endList();
        return null;
    }

    public Void visit(Ast.Array<Position> o) {
        printer.startList();
        posPrinter(o.a);
        printer.startList();
        printer.printAtom("TArray");

        o.t.accept(this);

        if (o.size.isPresent()) {
            printer.startList();
            o.size.get().accept(this);
            printer.endList();
        } else {
            printer.startList();
            printer.endList();
        }

        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.Use<Position> u) {
        printer.startList();
        posPrinter(u.a);
        printer.startList();
        printer.printAtom("Use");
        rawIdPrinter(u.x);
        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.Underscore<Position> u) {
        printer.startList();
        printer.printAtom("Underscore");
        posPrinter(u.a);
        printer.endList();
        return null;
    }

    public Void visit(Ast.FuncCall<Position> c) {
        printer.startList();
        posPrinter(c.a);
        printer.startList();
        printer.printAtom("FuncCall");

        rawIdPrinter(c.f);

        /* function arguments */
        printer.startList();
        c.args.forEach(v -> v.accept(this));
        printer.endList();

        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.ProcCall<Position> c) {
        printer.startList();
        posPrinter(c.a);
        printer.startList();
        printer.printAtom("ProcCall");

        rawIdPrinter(c.f);

        /* function arguments */
        printer.startList();
        c.args.forEach(v -> v.accept(this));
        printer.endList();

        printer.endList();
        printer.endList();
        return null;
    }

    public Void visit(Ast.ProcDecl<Position> p) {
        printer.startList();
        posPrinter(p.a);

        printer.startList();
        printer.printAtom("ProcDecl");

        rawIdPrinter(p.name);

        printer.startList();
        p.args.forEach(a -> a.accept(this));
        printer.endList();

        printer.endList();

        printer.endList();
        return null;
    }

    public Void visit(Ast.FuncDecl<Position> f) {
        printer.startList();
        posPrinter(f.a);

        printer.startList();
        printer.printAtom("FuncDecl");

        rawIdPrinter(f.name);

        printer.startList();
        f.args.forEach(a -> a.accept(this));
        printer.endList();

        printer.startList();
        f.returnType.forEach(t -> t.accept(this));
        printer.endList();

        printer.endList();

        printer.endList();
        return null;
    }

    public Void visit(Ast.Interface<Position> i) {
        printer.startList();
        posPrinter(i.a);

        printer.startList();
        printer.printAtom("Interface");

        printer.startList();
        i.fs.forEach(f -> f.accept(this));
        printer.endList();

        printer.endList();

        printer.endList();
        return null;
    }

    public Void visit(Ast.FullProgram<Position> p) {
        printer.startList();
        printer.printAtom("FullProg");

        p.prog.accept(this);

        printer.startList();
        p.inters.forEach(i -> i.accept(this));
        printer.endList();

        printer.endList();

        return null;
    }

    public void flush() {
        printer.flush();
    }
}
