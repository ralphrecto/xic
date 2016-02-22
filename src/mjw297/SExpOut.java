package mjw297;

import edu.cornell.cs.cs4120.util.CodeWriterSExpPrinter;
import edu.cornell.cs.cs4120.util.SExpPrinter;

import java.io.OutputStream;
import java.util.List;

class SExpOut implements Ast.NodeVisitor<Position, Void> {
    SExpPrinter printer;
    SExpOut(OutputStream o) {
        this.printer = new CodeWriterSExpPrinter(o);
    }

    public Void visit(Ast.AnnotatedId<Position> i) {
        printer.startList();
        i.x.accept(this);
        i.t.accept(this);
        printer.endList();

        return null;
    }

    public Void visit(Ast.AnnotatedUnderscore<Position> u) {
        printer.startList();
        u.u.accept(this);
        u.t.accept(this);
        printer.endList();

        return null;
    }

    public Void visit(Ast.Func<Position> f) {
        /* function name */
        printer.startList();
        f.name.accept(this);

        /* proc parameters */
        printer.startList();
        f.args.forEach(a -> a.accept(this));
        printer.endList();

        /* return type list is always empty */
        printer.startList();
        f.returnType.forEach(t -> t.accept(this));
        printer.endList();

        /* block */
        printer.startList();
        f.body.accept(this);

        /* returns */
        printer.printAtom("return");
        printer.endList();

        printer.endList();

        return null;
    }

    public Void visit(Ast.Proc<Position> p) {
        /* function name */
        printer.startList();
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
        printer.printAtom(i.x);

        return null;
    }

    public Void visit(Ast.BinOp<Position> o) {
        printer.startList();
        printer.printAtom(SymUtil.toTokenLiteral(o.c.name()));
        o.lhs.accept(this);
        o.rhs.accept(this);
        printer.endList();

        return null;
    }

    public Void visit(Ast.UnOp<Position> o) {
        printer.startList();
        printer.printAtom(SymUtil.toTokenLiteral(o.c.name()));
        o.e.accept(this);
        printer.endList();

        return null;
    }

    private void indexPrintHelper(Ast.Expr<Position> i,
      List<Ast.Expr<Position>> exprs) {
        if (exprs.size() == 0) {
            i.accept(this);
        } else {
            Ast.Expr<Position> last = exprs.get(exprs.size() - 1);
            exprs.remove(exprs.size() - 1);

            printer.startList();
            printer.printAtom("[]");
            indexPrintHelper(i, exprs);
            last.accept(this);
            printer.endList();
        }
    }

    public Void visit(Ast.Index<Position> i) {
        printer.startList();
        printer.printAtom("[]");
        i.e.accept(this);
        i.index.accept(this);
        printer.endList();
        return null;
    }

    public Void visit(Ast.Length<Position> l) {
        printer.startList();
        printer.printAtom("length");
        l.e.accept(this);
        printer.endList();

        return null;
    }

    public Void visit(Ast.NumLiteral<Position> n) {
        printer.printAtom(((Long) n.x).toString());

        return null;
    }

    public Void visit(Ast.BoolLiteral<Position> b) {
        printer.printAtom(((Boolean) b.b).toString());

        return null;
    }

    public Void visit(Ast.StringLiteral<Position> s) {
        printer.printAtom(String.format("\"%s\"", s.s));

        return null;
    }

    public Void visit(Ast.CharLiteral<Position> c) {
        printer.printAtom(
                String.format("\'%s\'", ((Character) c.c).toString())
        );
        return null;
    }

    public Void visit(Ast.ArrayLiteral<Position> a) {
        printer.startList();
        a.xs.forEach(e -> e.accept(this));
        printer.endList();
        return null;
    }

    public Void visit(Ast.Program<Position> p) {
        printer.startList();
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
        if (d.vs.size() > 1) {
            printer.startList();
            d.vs.forEach(v -> v.accept(this));
            printer.endList();
        } else {
            d.vs.forEach(v -> v.accept(this));
        }
        return null;
    }

    public Void visit(Ast.DeclAsgn<Position> d) {
        printer.startList();
        printer.printAtom("=");
        if (d.vs.size() > 1) {
            printer.startList();
            d.vs.forEach(v -> v.accept(this));
            printer.endList();
        } else {
            d.vs.forEach(v -> v.accept(this));
        }
        d.e.accept(this);
        printer.endList();
        return null;
    }

    public Void visit(Ast.Asgn<Position> a) {
        printer.startList();
        printer.printAtom("=");
        a.lhs.accept(this);
        a.rhs.accept(this);
        printer.endList();
        return null;
    }

    @Override
    public Void visit(Ast.Block<Position> b) {
        printer.startList();
        b.ss.forEach(s -> s.accept(this));
        if (b.ret.isPresent()) {
            printer.startList();
            printer.printAtom("return");
            b.ret.get().forEach(e -> e.accept(this));
            printer.endList();
        }
        printer.endList();
        return null;
    }

    public Void visit(Ast.If<Position> i) {
        printer.startList();
        printer.printAtom("if");

        /* predicate
        i.b.accept(this);

        /* block */
        i.body.accept(this);

        printer.endList();

        return null;
    }

    public Void visit(Ast.IfElse<Position> i) {
        printer.startList();
        printer.printAtom("if");

        i.b.accept(this);
        i.thenBody.accept(this);
        i.elseBody.accept(this);
        printer.endList();

        return null;
    }

    public Void visit(Ast.While<Position> w) {
        printer.startList();
        printer.printAtom("while");

        /* predicate */
        w.b.accept(this);

        /* body */
        w.body.accept(this);

        printer.endList();
        return null;
    }

    public Void visit(Ast.Int<Position> l) {
        printer.printAtom("int");
        return null;
    }

    public Void visit(Ast.Bool<Position> o) {
        printer.printAtom("bool");
        return null;
    }

    public Void visit(Ast.Array<Position> o) {
        printer.startList();
        if (o.size.isPresent()) {
            printer.printAtom("[]");
            o.t.accept(this);
            o.size.get().accept(this);
        } else {
            printer.printAtom("[]");
            o.t.accept(this);
        }
        printer.endList();
        return null;
    }

    public Void visit(Ast.Use<Position> u) {
        printer.startList();
        printer.printAtom("use");
        u.x.accept(this);
        printer.endList();
        return null;
    }

    public Void visit(Ast.Underscore<Position> u) {
        printer.printAtom("_");
        return null;
    }

    public Void visit(Ast.Call<Position> c) {
        printer.startList();
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


