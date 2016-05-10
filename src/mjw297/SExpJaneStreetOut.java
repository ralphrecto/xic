package mjw297;

import java.io.OutputStream;
import java.io.PrintWriter;

import mjw297.Ast.AnnotatedId;
import mjw297.Ast.AnnotatedUnderscore;
import mjw297.Ast.AnnotatedVar;
import mjw297.Ast.Underscore;

class SExpJaneStreetOut implements Ast.NodeVisitor<Position, Void> {
    PrintWriter printer;

    SExpJaneStreetOut(OutputStream o) {
        this.printer = new PrintWriter(o);
    }

    void startList()         { printer.write('(');     }
    void printAtom(String s) { printer.write(s + " "); }
    void endList()           { printer.write(')');     }
    private void posPrinter(Position p) {
        startList();
        printAtom(((Integer) p.row).toString());
        printAtom(((Integer) p.col).toString());
        endList();
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
        PrintWriter printer;

        void startList()         { printer.print('('); }
        void printAtom(String s) { printer.print(s);   }
        void endList()           { printer.print(')'); }

        SExpVar(SExpJaneStreetOut parent) {
            this.parent  = parent;
            this.printer = parent.printer;
        }

        @Override
        public Void visit(AnnotatedVar<Position> v) {
            startList();
            posPrinter(v.accept(new AnnotatedVarPos()));
            startList();
            printAtom("AVar");
            v.accept(this.parent);
            endList();
            endList();

            return null;
        }

        @Override
        public Void visit(Underscore<Position> u) {
            startList();
            posPrinter(u.a);
            printAtom("Underscore");
            endList();

            return null;
        }
    }

    private void rawIdPrinter(Ast.Id<Position> i) {
        startList();
        posPrinter(i.a);
        printAtom(i.x);
        endList();
    }

    public Void visit(Ast.AnnotatedId<Position> i) {
        startList();
        posPrinter(i.a);

        startList();
        printAtom("AId");

        rawIdPrinter(i.x);
        i.t.accept(this);

        endList();
        endList();

        return null;
    }

    public Void visit(Ast.AnnotatedUnderscore<Position> u) {
        startList();
        posPrinter(u.a);
        startList();
        printAtom("AUnderscore");
        u.t.accept(this);
        endList();
        endList();

        return null;
    }

    public Void visit(Ast.Func<Position> f) {
        /* function name */
        startList();
        posPrinter(f.a);

        startList();
        printAtom("Func");
        rawIdPrinter(f.name);

        /* proc parameters */
        startList();
        f.args.forEach(a -> a.accept(this));
        endList();

        /* return types */
        startList();
        f.returnType.forEach(t -> t.accept(this));
        endList();

        f.body.accept(this);

        endList();
        endList();

        return null;
    }

    public Void visit(Ast.Proc<Position> p) {
        /* function name */
        startList();
        posPrinter(p.a);

        startList();
        printAtom("Proc");

        rawIdPrinter(p.name);

        /* proc parameters */
        startList();
        p.args.forEach(av -> av.accept(this));
        endList();

        /* block */
        p.body.accept(this);

        endList();
        endList();

        return null;
    }

    public Void visit(Ast.Klass<Position> p) {
        return null;
    }

    public Void visit(Ast.Id<Position> i) {
        startList();
        posPrinter(i.a);
        startList();
        printAtom("Id");
        rawIdPrinter(i);
        endList();
        endList();

        return null;
    }

    public Void visit(Ast.BinOp<Position> o) {
        startList();
        posPrinter(o.a);
        startList();
        printAtom("BinOp");
        o.lhs.accept(this);
        printAtom(o.c.name());
        o.rhs.accept(this);
        endList();
        endList();

        return null;
    }

    public Void visit(Ast.UnOp<Position> o) {
        startList();
        posPrinter(o.a);
        startList();
        printAtom("UnOp");
        printAtom(o.c.name());
        o.e.accept(this);
        endList();
        endList();

        return null;
    }

    public Void visit(Ast.Index<Position> i) {
        startList();
        posPrinter(i.a);
        startList();
        printAtom("Index");
        i.e.accept(this);
        i.index.accept(this);
        endList();
        endList();
        return null;
    }

    public Void visit(Ast.Length<Position> l) {
        startList();
        posPrinter(l.a);
        startList();
        printAtom("Length");
        l.e.accept(this);
        endList();
        endList();

        return null;
    }

    public Void visit(Ast.NumLiteral<Position> n) {
        startList();
        posPrinter(n.a);
        startList();
        printAtom("Int");
        printAtom(((Long) n.x).toString());
        endList();
        endList();

        return null;
    }

    public Void visit(Ast.BoolLiteral<Position> b) {
        startList();
        posPrinter(b.a);
        startList();
        printAtom("Bool");
        printAtom(((Boolean) b.b).toString());
        endList();
        endList();

        return null;
    }

    public Void visit(Ast.StringLiteral<Position> s) {
        startList();
        posPrinter(s.a);
        startList();
        printAtom("String");
        printAtom(
                String.format("\"%s\"", SymUtil.prettyPrintString(s.s))
        );
        endList();
        endList();
        return null;
    }

    public Void visit(Ast.CharLiteral<Position> c) {
        startList();
        posPrinter(c.a);
        startList();
        printAtom("Char");
        printAtom(
                String.format("%s", SymUtil.prettyPrintChar(c.c))
        );
        endList();
        endList();
        return null;
    }

    public Void visit(Ast.ArrayLiteral<Position> a) {
        startList();
        posPrinter(a.a);
        startList();
        printAtom("Array");

        /* Actual list contents */
        startList();
        a.xs.forEach(e -> e.accept(this));
        endList();

        endList();
        endList();
        return null;
    }

    public Void visit(Ast.NullLiteral<Position> a) {
        return null;
    }

    public Void visit(Ast.Program<Position> p) {
        startList();
        posPrinter(p.a);

        startList();
        printAtom("Prog");

        startList();
        p.uses.forEach((u -> u.accept(this)));
        endList();

        startList();
        p.fs.forEach(f -> f.accept(this));
        endList();

        endList();
        endList();
        return null;
    }

    public Void visit(Ast.Decl<Position> d) {
        startList();
        posPrinter(d.a);
        startList();
        printAtom("Decl");

        startList();
        SExpVar varVisitor = new SExpVar(this);
        d.vs.forEach(v -> v.accept(varVisitor));
        endList();

        endList();
        endList();

        return null;
    }

    public Void visit(Ast.DeclAsgn<Position> d) {
        startList();
        posPrinter(d.a);
        startList();
        printAtom("DeclAsgn");

        startList();
        SExpVar varVisitor = new SExpVar(this);
        d.vs.forEach(v -> v.accept(varVisitor));
        endList();

        d.e.accept(this);

        endList();
        endList();
        return null;
    }

    public Void visit(Ast.MultiDecl<Position> d) {
        return null;
    }

    public Void visit(Ast.Asgn<Position> a) {
        startList();
        posPrinter(a.a);
        startList();
        printAtom("Asgn");

        a.lhs.accept(this);
        a.rhs.accept(this);

        endList();
        endList();
        return null;
    }

    public Void visit(Ast.UnderscoreAsgn<Position> a) {
        return null;
    }

    @Override
    public Void visit(Ast.Block<Position> b) {
        startList();
        posPrinter(b.a);

        startList();
        printAtom("Block");

        /* stmt list */
        startList();
        b.ss.forEach(s -> s.accept(this));
        if (b.ret.isPresent()) {
            startList();
            posPrinter(b.ret_a);

            startList();
            printAtom("Return");

            startList();
            b.ret.get().forEach(e -> e.accept(this));
            endList();

            endList();

            endList();
        }
        endList();

        endList();

        endList();
        return null;
    }

    public Void visit(Ast.If<Position> i) {
        startList();
        posPrinter(i.a);
        startList();
        printAtom("If");

        i.b.accept(this);
        i.body.accept(this);

        endList();
        endList();

        return null;
    }

    public Void visit(Ast.IfElse<Position> i) {
        startList();
        posPrinter(i.a);
        startList();
        printAtom("IfElse");

        i.b.accept(this);
        i.thenBody.accept(this);
        i.elseBody.accept(this);

        endList();
        endList();
        return null;
    }

    public Void visit(Ast.While<Position> w) {
        startList();
        posPrinter(w.a);
        startList();
        printAtom("While");

        /* predicate */
        w.b.accept(this);

        /* body */
        w.body.accept(this);

        endList();
        endList();
        return null;
    }

    public Void visit(Ast.Int<Position> l) {
        startList();
        posPrinter(l.a);
        printAtom("TInt");
        endList();
        return null;
    }

    public Void visit(Ast.Bool<Position> o) {
        startList();
        posPrinter(o.a);
        printAtom("TBool");
        endList();
        return null;
    }

    public Void visit(Ast.Array<Position> o) {
        startList();
        posPrinter(o.a);
        startList();
        printAtom("TArray");

        o.t.accept(this);

        if (o.size.isPresent()) {
            startList();
            o.size.get().accept(this);
            endList();
        } else {
            startList();
            endList();
        }

        endList();
        endList();
        return null;
    }

    public Void visit(Ast.KlassType<Position> o) {
        // TODO
        return null;
    }

    public Void visit(Ast.Use<Position> u) {
        startList();
        posPrinter(u.a);
        startList();
        printAtom("Use");
        rawIdPrinter(u.x);
        endList();
        endList();
        return null;
    }

    public Void visit(Ast.Underscore<Position> u) {
        startList();
        printAtom("Underscore");
        posPrinter(u.a);
        endList();
        return null;
    }

    public Void visit(Ast.FuncCall<Position> c) {
        startList();
        posPrinter(c.a);
        startList();
        printAtom("FuncCall");

        rawIdPrinter(c.f);

        /* function arguments */
        startList();
        c.args.forEach(v -> v.accept(this));
        endList();

        endList();
        endList();
        return null;
    }

    public Void visit(Ast.New<Position> c) {
        return null;
    }

    public Void visit(Ast.ProcCall<Position> c) {
        startList();
        posPrinter(c.a);
        startList();
        printAtom("ProcCall");

        rawIdPrinter(c.f);

        /* function arguments */
        startList();
        c.args.forEach(v -> v.accept(this));
        endList();

        endList();
        endList();
        return null;
    }

    public Void visit(Ast.Break<Position> c) {
        // TODO: complete this
        return null;
    }

    public Void visit(Ast.ProcDecl<Position> p) {
        startList();
        posPrinter(p.a);

        startList();
        printAtom("ProcDecl");

        rawIdPrinter(p.name);

        startList();
        p.args.forEach(a -> a.accept(this));
        endList();

        endList();

        endList();
        return null;
    }

    public Void visit(Ast.KlassDecl<Position> p) {
        return null;
    }

    public Void visit(Ast.FuncDecl<Position> f) {
        startList();
        posPrinter(f.a);

        startList();
        printAtom("FuncDecl");

        rawIdPrinter(f.name);

        startList();
        f.args.forEach(a -> a.accept(this));
        endList();

        startList();
        f.returnType.forEach(t -> t.accept(this));
        endList();

        endList();

        endList();
        return null;
    }

    public Void visit(Ast.Interface<Position> i) {
        startList();
        posPrinter(i.a);

        startList();
        printAtom("Interface");

        startList();
        i.fs.forEach(f -> f.accept(this));
        endList();

        endList();

        endList();
        return null;
    }

    public Void visit(Ast.FullProgram<Position> p) {
        startList();
        printAtom("FullProg");

        printAtom(p.progname);

        p.prog.accept(this);

        startList();
        p.inters.forEach(i -> i.accept(this));
        endList();

        endList();

        return null;
    }

    public Void visit(Ast.MethodCallStmt<Position> m) {
        // TODO
        return null;
    }

    public Void visit(Ast.MethodCall<Position> m) {
        // TODO
        return null;
    }

    public Void visit(Ast.FieldAccess<Position> m) {
        // TODO
        return null;
    }

    public void flush() {
        printer.flush();
    }
}
