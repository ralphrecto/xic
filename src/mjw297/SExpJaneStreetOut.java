package mjw297;

import edu.cornell.cs.cs4120.util.CodeWriterSExpPrinter;
import edu.cornell.cs.cs4120.util.SExpPrinter;

import mjw297.Either.*;
import mjw297.Ast.*;

import java.io.OutputStream;
import java.util.List;

class SExpJaneStreetOut implements Ast.NodeVisitor<Position, Void> {
    SExpPrinter printer;
    SExpJaneStreetOut(OutputStream o) {
        this.printer = new CodeWriterSExpPrinter(o);
    }

    private void posPrinter(Position p) {
        this.printer.startList();
        this.printer.printAtom("Position");
        this.printer.printAtom(((Integer) p.row).toString());
        this.printer.printAtom(((Integer) p.col).toString());
        this.printer.endList();
    }

    public Void visit(Ast.AnnotatedId<Position> i) {
        printer.startList();
        printer.printAtom("AnnotatedId");
        posPrinter(i.a);
        i.x.accept(this);
        i.t.accept(this);
        printer.endList();

        return null;
    }

    public Void visit(Ast.AnnotatedUnderscore<Position> u) {
        printer.startList();
        printer.printAtom("AnnotatedUnderscore");
        posPrinter(u.a);
        u.u.accept(this);
        u.t.accept(this);
        printer.endList();

        return null;
    }

    public Void visit(Ast.Func<Position> f) {
        /* function name */
        printer.startList();
        printer.printAtom("Func");
        posPrinter(f.a);
        f.name.accept(this);

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

        return null;
    }

    public Void visit(Ast.Proc<Position> p) {
        /* function name */
        printer.startList();
        printer.printAtom("Proc");
        posPrinter(p.a);

        p.name.accept(this);

        /* proc parameters */
        printer.startList();
        p.args.forEach(av -> av.accept(this));
        printer.endList();

        /* return type list is always empty */
        printer.startList();
        printer.endList();

        /* block */
        p.body.accept(this);

        printer.endList();

        return null;
    }

    public Void visit(Ast.Id<Position> i) {
        printer.startList();
        printer.printAtom("Id");
        posPrinter(i.a);
        printer.printAtom(i.x);
        printer.endList();

        return null;
    }

    public Void visit(Ast.BinOp<Position> o) {
        printer.startList();
        printer.printAtom("BinOp");
        posPrinter(o.a);
        printer.printAtom(SymUtil.toTokenLiteral(o.c.name()));
        o.lhs.accept(this);
        o.rhs.accept(this);
        printer.endList();

        return null;
    }

    public Void visit(Ast.UnOp<Position> o) {
        printer.startList();
        printer.printAtom("UnOp");
        posPrinter(o.a);
        printer.printAtom(SymUtil.toTokenLiteral(o.c.name()));
        o.e.accept(this);
        printer.endList();

        return null;
    }

    public Void visit(Ast.Index<Position> i) {
        printer.startList();
        printer.printAtom("Index");
        posPrinter(i.a);
        i.e.accept(this);
        i.index.accept(this);
        printer.endList();
        return null;
    }

    public Void visit(Ast.Length<Position> l) {
        printer.startList();
        printer.printAtom("Length");
        posPrinter(l.a);
        l.e.accept(this);
        printer.endList();

        return null;
    }

    public Void visit(Ast.NumLiteral<Position> n) {
        printer.startList();
        printer.printAtom("NumLiteral");
        posPrinter(n.a);
        printer.printAtom(((Long) n.x).toString());
        printer.endList();

        return null;
    }

    public Void visit(Ast.BoolLiteral<Position> b) {
        printer.startList();
        printer.printAtom("BoolLiteral");
        posPrinter(b.a);
        printer.printAtom(((Boolean) b.b).toString());
        printer.endList();

        return null;
    }

    public Void visit(Ast.StringLiteral<Position> s) {
        printer.startList();
        printer.printAtom("StringLiteral");
        posPrinter(s.a);
        printer.printAtom(
                String.format("\"%s\"", SymUtil.prettyPrintString(s.s))
        );
        printer.endList();
        return null;
    }

    public Void visit(Ast.CharLiteral<Position> c) {
        printer.startList();
        printer.printAtom("CharLiteral");
        posPrinter(c.a);
        printer.printAtom(
                String.format("\'%s\'", SymUtil.prettyPrintChar(c.c))
        );
        printer.endList();
        return null;
    }

    public Void visit(Ast.ArrayLiteral<Position> a) {
        printer.startList();
        printer.printAtom("ArrayLiteral");
        posPrinter(a.a);

        /* Actual list contents */
        printer.startList();
        a.xs.forEach(e -> e.accept(this));
        printer.endList();

        printer.endList();
        return null;
    }

    public Void visit(Ast.Program<Position> p) {
        printer.startList();
        printer.printAtom("Program");
        posPrinter(p.a);

        printer.startList();
        p.uses.forEach((u -> u.accept(this)));
        printer.endList();

        printer.startList();
        p.fs.forEach(f -> f.accept(this));
        printer.endList();

        printer.endList();
        return null;
    }

    public Void visit(Ast.Decl<Position> d) {
        printer.startList();
        printer.printAtom("Decl");
        posPrinter(d.a);

        printer.startList();
        d.vs.forEach(v -> v.accept(this));
        printer.endList();

        printer.endList();

        return null;
    }

    public Void visit(Ast.DeclAsgn<Position> d) {
        printer.startList();
        printer.printAtom("DeclAsgn");
        posPrinter(d.a);

        printer.startList();
        d.vs.forEach(v -> v.accept(this));
        printer.endList();

        printer.endList();
        return null;
    }

    public Void visit(Ast.Asgn<Position> a) {
        printer.startList();
        printer.printAtom("Asgn");
        posPrinter(a.a);

        a.lhs.accept(this);
        a.rhs.accept(this);

        printer.endList();
        return null;
    }

    public Void visit(Ast.UnderscoreAsgn<Position> a) {
        printer.startList();
        printer.printAtom("UnderscoreAsgn");
        posPrinter(a.a);

        a.lhs.accept(this);
        a.rhs.accept(this);

        printer.endList();
        return null;
    }

    @Override
    public Void visit(Ast.Block<Position> b) {
        printer.startList();
        printer.printAtom("Block");
        posPrinter(b.a);

        printer.startList();
        b.ss.forEach(s -> s.accept(this));
        printer.endList();

        if (b.ret.isPresent()) {
            printer.startList();
            printer.printAtom("some");

            printer.startList();
            b.ret.get().forEach(e -> e.accept(this));
            printer.endList();

            printer.endList();
        } else {
            printer.printAtom("none");
        }

        printer.endList();
        return null;
    }

    public Void visit(Ast.If<Position> i) {
        printer.startList();
        printer.printAtom("If");
        posPrinter(i.a);

        i.b.accept(this);
        i.body.accept(this);

        printer.endList();

        return null;
    }

    public Void visit(Ast.IfElse<Position> i) {
        printer.startList();
        printer.printAtom("IfElse");
        posPrinter(i.a);

        i.b.accept(this);
        i.thenBody.accept(this);
        i.elseBody.accept(this);

        printer.endList();
        return null;
    }

    public Void visit(Ast.While<Position> w) {
        printer.startList();
        printer.printAtom("While");
        posPrinter(w.a);

        /* predicate */
        w.b.accept(this);

        /* body */
        w.body.accept(this);

        printer.endList();
        return null;
    }

    public Void visit(Ast.Int<Position> l) {
        printer.startList();
        printer.printAtom("Int");
        posPrinter(l.a);
        printer.endList();
        return null;
    }

    public Void visit(Ast.Bool<Position> o) {
        printer.startList();
        printer.printAtom("Bool");
        posPrinter(l.a);
        printer.endList();
        return null;
    }

    public Void visit(Ast.Array<Position> o) {
        printer.startList();
        printer.printAtom("Array");
        posPrinter(o.a);

        o.t.accept(this);

        if (o.size.isPresent()) {
            printer.startList();
            printer.printAtom("some");
            o.size.get().accept(this);
            printer.endList();
        } else {
            printer.printAtom("none");
        }

        printer.endList();
        return null;
    }

    public Void visit(Ast.Use<Position> u) {
        printer.startList();
        printer.printAtom("Use");
        posPrinter(u.a);
        u.x.accept(this);
        printer.endList();
        return null;
    }

    public Void visit(Ast.Underscore<Position> u) {
        printer.startList();
        printer.printAtom("Underscore");
        printer.endList();
        return null;
    }

    public Void visit(Ast.Call<Position> c) {
        printer.startList();
        printer.printAtom("Call");
        posPrinter(c.a);

        c.f.accept(this);

        /* function arguments */
        c.args.forEach(v -> v.accept(this));

        printer.endList();
        return null;
    }

    public void flush() {
        printer.flush();
    }
}


