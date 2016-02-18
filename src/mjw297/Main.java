package mjw297;
import com.google.common.collect.Lists;
import edu.cornell.cs.cs4120.util.CodeWriterSExpPrinter;
import edu.cornell.cs.cs4120.util.SExpPrinter;
import java_cup.runtime.Symbol;
import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;
import com.google.common.io.Files;
import polyglot.util.CodeWriter;
import polyglot.util.OptimalCodeWriter;

import java.io.*;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * The main compiler frontend/CLI interface to the compiler.
 */
public class Main {

    @Option(name="--help", usage="Print a synopsis of options.")
    private static boolean helpMode = false;

    @Option(name="--lex", usage="Generate output from lexical analysis")
    private static boolean lexMode = false;

    @Option(name="--parse", usage="Generate output from syntactic analysis")
    private static boolean parseMode = false;

    @Option(name="-sourcepath", usage="Specify where to find input source files")
    private static String sourcePath = "";

    @Option(name="-D", usage="Specify where to place generated diagnostic files")
    private static String diagnosticPath = "";

    @Argument(usage="Other non-optional arguments.", hidden=true)
    private static List<String> arguments = new ArrayList<String>();

    private CmdLineParser parser;

    public Main() {
        this.parser = new CmdLineParser(this);
    }

    /**
     * {@code XiSource} represents a Xi Source file. Any instance of this
     * necessarily has a .xi extension
     */
    static class XiSource {

        @SuppressWarnings("serial")
        static class XiSourceException extends Exception {
            XiSourceException(String msg) { super(msg); }
        }

        String filename;
        FileReader reader;

        private XiSource(String filename, FileReader reader) {
            this.filename = filename;
            this.reader = reader;
        }

        /**
         * Change the source's extension. Use for output files that
         * will live in the same directory.
         */
        String changeExtension(String newExt) {
            return String.format(
                    "%s.%s", filename.substring(0, filename.length() - 3),
                    newExt
            );
        }

        static XiSource create(String filename) throws XiSourceException {
            if (!Files.getFileExtension(filename).equals("xi")) {
                throw new XiSourceException("Valid Xi files must have .xi extension");
            }
            try {
                return new XiSource(filename, new FileReader(sourcePath + filename));
            } catch (FileNotFoundException e) {
                throw new XiSourceException(e.getMessage());
            }
        }

        static List<XiSource> createMany(List<String> filenames) throws XiSourceException {
            List<XiSource> sources = new ArrayList<>();
            for (String filename : filenames) {
                sources.add(create(filename));
            }
            return sources;
        }
    }

    /** Helper class to compose stages of the compiler together
     * as transformations over Xi source files */
    private static class Staging {
        private List<XiSource> sources;

        /* These filenames are relative to sourcepath;
         * XiSources keep the passed filenames */
        Staging(List<String> filenames) {
            try {
                sources = XiSource.createMany(filenames);
            } catch (XiSource.XiSourceException e) {
                System.out.println(e.getMessage());
                System.exit(1);
            }
        }

        private List<Actions.Lexed> lexS() {
            return Lists.transform(sources, xs -> {
                Actions.Lexed lexed = null;
                try {
                    lexed = Actions.lex(xs.reader);
                } catch (IOException e) {
                    e.printStackTrace();
                    System.exit(1);
                }
                return lexed;
            });
        }

        List<Util.Tuple<Actions.Lexed, XiSource>> lex() {
            return Util.zip(lexS(), this.sources);
        }

        private List<Actions.Parsed> parseS() {
            return Lists.transform(sources, xs -> {
                Actions.Parsed parsed = null;
                try {
                    parsed = Actions.parse(xs.reader);
                } catch (Exception e) {
                    e.printStackTrace();
                    System.exit(1);
                }
                return parsed;
            });
        }

        List<Util.Tuple<Actions.Parsed, XiSource>> parse() {
            return Util.zip(parseS(), this.sources);
        }
    }

    /**
     * Helper function to print binary usage info
     */
    private void printUsage() {
        System.out.println("xic [options] <source files>");
        parser.printUsage(System.err);
    }

    /**
     * Actions for the --lex option
     */
    private void lexOut(List<Util.Tuple<Actions.Lexed, XiSource>> lexed) {
        for (Util.Tuple<Actions.Lexed, XiSource> t : lexed) {
            StringBuilder outputBuilder = new StringBuilder();

            for (Symbol sym : t.fst.symbols) {
                outputBuilder.append(
                    String.format("%d:%d %s\n", sym.left, sym.right,
                        SymUtil.symToLiteral(sym)
                    )
                );
            }

            if (t.fst.exception.isPresent()) {
                XicException e = t.fst.exception.get();
                outputBuilder.append(
                    String.format("%d:%d %s\n", e.row, e.column, e.getMessage())
                );
            }


            String outputFilename = diagnosticPath + t.snd.changeExtension("lexed");
            File outputFile = Paths.get(outputFilename).toFile();

            try {
                Files.write(outputBuilder.toString().getBytes(), outputFile);
            } catch (IOException e) {
                e.printStackTrace();
                System.out.println("Could not write to file " + outputFilename);
                return;
            }
        }
    }

    void parseOut(List<Util.Tuple<Actions.Parsed, XiSource>> parsed) {

        class SExpOut implements Ast.NodeVisitor<Void> {
            SExpPrinter printer;
            SExpOut(SExpPrinter printer) {
                this.printer = printer;
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
				/* RALPH IS DUMB THE RETURN TYPE IS NOT ALWAYS EMPTY */
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
                printer.startList();

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

			public Void visit(Ast.ParenthesizedExpr e) {
                 /* TODO */
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
                printer.printAtom("[]");
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

        for (Util.Tuple<Actions.Parsed, XiSource> p : parsed) {
            SExpPrinter printer = new CodeWriterSExpPrinter(System.out);
            SExpOut sExpOut = new SExpOut(printer);
            ((Ast.Program) p.fst.prog.value).accept(sExpOut);
            sExpOut.flush();
            System.out.println("");
        }
    }

    /**
     * Main entry point. Handles actions for the different CLI options.
     * @param args The command line arguments
     */
    private void doMain(String[] args) {

        try {
            parser.parseArgument(args);

            if (arguments.isEmpty()) {
                System.out.println("No filenames provided.");
                printUsage();
                System.exit(1);
            }

            if (sourcePath.equals("")) {
                sourcePath = Paths.get(".").toAbsolutePath().toString();
            }

            if (diagnosticPath.equals("")) {
                diagnosticPath = Paths.get(".").toAbsolutePath().toString();
            }

            sourcePath = Files.simplifyPath(sourcePath) + "/";
            diagnosticPath = Files.simplifyPath(diagnosticPath) + "/";

            Staging staging = new Staging(arguments);

            if (helpMode) {
                printUsage();
            } else if (lexMode) {
                lexOut(staging.lex());
            } else if (parseMode) {
                parseOut(staging.parse());
            } else {
                System.out.println("No options passed.");
                printUsage();
            }

        } catch(CmdLineException e) {
            System.err.println(e.getMessage());
            printUsage();
        }
    }

    public static void main(String[] args) {
        new Main().doMain(args);
    }
}
