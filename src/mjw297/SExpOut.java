package mjw297;

import edu.cornell.cs.cs4120.util.CodeWriterSExpPrinter;
import edu.cornell.cs.cs4120.util.SExpPrinter;
import polyglot.util.OptimalCodeWriter;

import java.io.OutputStream;

/**
 * Created by ralphrecto on 2/20/16.
 */

class SExpOut implements Ast.NodeVisitor<Void> {
        SExpPrinter printer;
        SExpOut(OutputStream o) {
            this.printer = new CodeWriterSExpPrinter(o);
        }

        public Void visit(Ast.AnnotatedId i) {
                printer.startList();
                i.x.accept(this);
                i.t.accept(this);
                printer.endList();

                return null;
        }

        public Void visit(Ast.AnnotatedUnderscore u) {
                printer.startList();
                u.u.accept(this);
                u.t.accept(this);
                printer.endList();

                return null;
        }

        public Void visit(Ast.Func f) {
                /* function name */
                printer.startList();
                f.name.accept(this);

                /* proc parameters */
                printer.startList();
                f.args.forEach(av -> av.accept(this));
                printer.endList();

                /* return type list is always empty */
                printer.startList();
                f.returnType.forEach(t -> t.accept(this));
                printer.endList();

                /* block */
                printer.startList();
                f.body.forEach(s -> s.accept(this));

				/* returns */
                printer.printAtom("return");
                f.returns.forEach(e -> e.accept(this));
                printer.endList();

                printer.endList();

                return null;
        }

        public Void visit(Ast.Proc p) {
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
                p.body.forEach(s -> s.accept(this));

                printer.endList();

                return null;
        }

        public Void visit(Ast.Id i) {
                printer.printAtom(i.x);

                return null;
        }

        public Void visit(Ast.BinOp o) {
                printer.startList();
                printer.printAtom(o.c.toString());
                o.lhs.accept(this);
                o.rhs.accept(this);
                printer.endList();

                return null;
        }

        public Void visit(Ast.UnOp o) {
                printer.startList();
                printer.printAtom(o.c.toString());
                o.e.accept(this);
                printer.endList();

                return null;
        }

        public Void visit(Ast.Index i) {
                /* TODO */
                return null;
        }

        public Void visit(Ast.Length l) {
                printer.startList();
                printer.printAtom("length");
                l.e.accept(this);
                printer.endList();

                return null;
        }

        public Void visit(Ast.NumLiteral n) {
                printer.printAtom(((Long) n.x).toString());

                return null;
        }

        public Void visit(Ast.BoolLiteral b) {
                printer.printAtom(((Boolean) b.b).toString());

                return null;
        }

        public Void visit(Ast.StringLiteral s) {
                printer.printAtom(s.s);

                return null;
        }

        public Void visit(Ast.CharLiteral c) {
                printer.printAtom(((Character) c.c).toString());
                return null;
        }

        public Void visit(Ast.ArrayLiteral a) {
                printer.startList();
                a.xs.forEach(e -> e.accept(this));
                printer.endList();
                return null;
        }

        public Void visit(Ast.Program p) {
                p.uses.forEach((u -> u.accept(this)));
                p.fs.forEach(f -> f.accept(this));
                return null;
        }

        public Void visit(Ast.Decl d) {
                printer.startList();
                d.vs.forEach(v -> v.accept(this));
                printer.endList();
                return null;
        }

        public Void visit(Ast.DeclAsgn d) {
                printer.startList();
                printer.printAtom("=");
                d.vs.forEach(v -> v.accept(this));
                d.e.accept(this);
                printer.endList();
                return null;
        }

        public Void visit(Ast.Asgn a) {
                printer.startList();
                printer.printAtom("=");
                a.id.accept(this);
                a.expr.accept(this);
                printer.endList();
                return null;
        }

        public Void visit(Ast.AsgnArrayIndex a) {
                /* TODO */
                return null;
        }

        public Void visit(Ast.If i) {
                printer.startList();
                printer.printAtom("if");

                /* predicate
                printer.startList();*/
                i.b.accept(this);
                //printer.endList();

                /* block */
                printer.startList();
                i.body.forEach(s -> s.accept(this));
                printer.endList();

                printer.endList();

                return null;
        }

        public Void visit(Ast.IfElse i) {
                printer.startList();
                printer.printAtom("if");

                /* predicate */
                printer.startList();
                i.b.accept(this);
                printer.endList();

                /* then block */
                printer.startList();
                i.thenBody.forEach(s -> s.accept(this));
                printer.endList();

                /* else block */
                printer.startList();
                i.elseBody.forEach(s -> s.accept(this));
                printer.endList();

                printer.endList();

                return null;
        }

        public Void visit(Ast.While w) {
                printer.startList();
                printer.printAtom("while");

                /* predicate */
                printer.startList();
                w.b.accept(this);
                printer.endList();

                /* body */
                printer.startList();
                w.body.forEach(s -> s.accept(this));
                printer.endList();

                printer.endList();
                return null;
        }

        public Void visit(Ast.Int l) {
                printer.printAtom("int");
                return null;
        }

        public Void visit(Ast.Bool o) {
                printer.printAtom("bool");
                return null;
        }

        public Void visit(Ast.Array o) {
                /* TODO: incorporate size */
                printer.startList();
                if (o.size.isPresent()) {
                    printer.printAtom("[");
                    o.size.get().accept(this);
                    printer.printAtom("]");
                } else {
                    printer.printAtom("[]");
                }
                o.t.accept(this);
                printer.endList();
                return null;
        }

        public Void visit(Ast.Use u) {
                printer.startList();
                printer.printAtom("use");
                u.x.accept(this);
                printer.endList();
                return null;
        }

        public Void visit(Ast.Underscore u) {
                printer.printAtom("_");
                return null;
        }

        public Void visit(Ast.Call c) {
                printer.startList();
                c.f.accept(this);

                /* function arguments */
                printer.startList();
                c.args.forEach(v -> v.accept(this));
                printer.endList();

                printer.endList();
                return null;
        }

        public void flush() {
                printer.flush();
        }
}

