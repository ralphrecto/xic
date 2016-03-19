package mjw297;
import com.google.common.collect.Lists;
import java_cup.runtime.Symbol;
import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;
import com.google.common.io.Files;

import java.io.*;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static mjw297.Util.Tuple;
import static mjw297.Util.Either;
import static mjw297.Ast.*;
import static mjw297.Actions.*;
import static mjw297.XicException.*;

/**
 * The main compiler frontend/CLI interface to the compiler.
 */
public class Main {

    @Option(name = "--help", usage = "Print a synopsis of options.")
    private static boolean helpMode = false;

    @Option(name = "--lex", usage = "Generate output from lexical analysis")
    private static boolean lexMode = false;

    @Option(name = "--parse", usage = "Generate output from syntactic analysis")
    private static boolean parseMode = false;

    @Option(name = "--typecheck", usage = "Generate output from syntactic analysis")
    private static boolean typecheckMode = false;

    @Option(name = "-sourcepath", usage = "Specify where to find input source files")
    private static String sourcePath = "";

    @Option(name = "-libpath", usage = "Specify where to find input source files")
    private static String libPath = "";

    @Option(name = "-D", usage = "Specify where to place generated diagnostic files")
    private static String diagnosticPath = "";

    @Option(name = "-compilerpath", hidden = true, required = true)
    private static String compilerPath;

    @Argument(usage = "Other non-optional arguments.", hidden = true)
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
            XiSourceException(String msg) {
                super(msg);
            }
        }

        String filename;
        FileReader reader;
        File file;

        private XiSource(String filename, File f, FileReader reader) {
            this.filename = filename;
            this.reader = reader;
            this.file = f;
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

        static XiSource create(String baseDir, String filename) {
            String ext = Files.getFileExtension(filename);
            if (!(ext.equals("xi") || ext.equals("ixi"))) {
                System.out.println("Valid Xi files must have .xi or .ixi extension");
                System.exit(1);
            }
            try {
                File f = Paths.get(baseDir, filename).toFile();
                return new XiSource(filename, f, new FileReader(f));
            } catch (FileNotFoundException e) {
                System.out.println(e.getMessage());
                System.exit(1);
            }
            return null;
        }

        static XiSource create(String filename) {
            return create(sourcePath, filename);
        }

        static List<XiSource> createMany(String baseDir, List<String> filenames) {
            List<XiSource> sources = new ArrayList<>();
            for (String filename : filenames) {
                sources.add(create(baseDir, filename));
            }
            return sources;
        }

        static List<XiSource> createMany(List<String> filenames) {
            return createMany(sourcePath, filenames);
        }

    }

    /**
     * Helper function to print binary usage info
     */
    private void printUsage() {
        System.out.println("xic [options] <source files>");
        parser.printUsage(System.err);
    }

    private String diagPathOut(XiSource xs, String ext) {
        String nameNoExt = Files.getNameWithoutExtension(xs.filename);
        String parentDir = xs.file.getParent();
        if (diagnosticPath.equals("")) {
            if (parentDir == null) {
                return String.format(xs.changeExtension(ext));
            } else {
                return String.format(
                        "%s/%s.%s",
                        Files.simplifyPath(parentDir),
                        nameNoExt,
                        ext
                );
            }
        } else {
            return String.format(
                    "%s/%s.%s",
                    Files.simplifyPath(diagnosticPath),
                    nameNoExt,
                    ext
            );
        }
    }

    private void writeToFile(String filename, String contents) {
        writeToFile(Paths.get(filename).toFile(), contents);
    }

    private void writeToFile(File file, String contents) {
        try {
            Files.write(contents.getBytes(), file);
        } catch (IOException e) {
            System.out.println(String.format(
                "Cannot write to file %s",
                file.getAbsolutePath()
            ));
            System.exit(1);
        }
    }

    private FileOutputStream getFileOutputStream(String filename) {
        try {
            return new FileOutputStream(Paths.get(filename).toFile());
        } catch (FileNotFoundException e) {
            System.out.println(String.format(
                "Cannot open file %s",
                filename
            ));
            System.exit(1);
        }
        return null;
    }

    /**
     * Actions for the --lex option
     */
    private void doLex(List<String> filenames) {
        List<XiSource> sources = XiSource.createMany(filenames);

        List<Tuple<Lexed, XiSource>> lexedOut = Lists.transform(sources,
            xs -> Tuple.of(Actions.lex(xs.reader), xs)
        );

        for (Tuple<Lexed, XiSource> t : lexedOut) {
            Lexed lexed = t.fst;
            StringBuilder outputBuilder = new StringBuilder();

            for (int i = 0; i < lexed.symbols.size(); i++) {
                Symbol sym = lexed.symbols.get(i);
                if (sym.sym == Sym.EOF) continue;
                outputBuilder.append(
                    String.format("%d:%d %s\n", sym.left, sym.right,
                        SymUtil.symToLiteral(sym)
                    )
                );
            }

            if (t.fst.exception.isPresent()) {
                XicException e = t.fst.exception.get();
                System.out.println(String.format(
                    "Lexical error at %s beginning at %d:%d: %s",
                    t.snd.filename, e.row, e.column, e.getMessage()
                ));
                outputBuilder.append(
                    String.format("%d:%d %s\n", e.row, e.column, e.getMessage())
                );
            }

            String outputFilename = diagPathOut(t.snd, "lexed");
            writeToFile(outputFilename, outputBuilder.toString());
        }
    }

    void writeParseError(XicException e, String filename, String outputFilename) {
        String kind = e.type == ErrorType.LEXING ? "Lexical" : "Syntactic";
        System.out.println(String.format(
            "%s error in %s beginning at %d:%d: %s",
            kind, filename, e.row, e.column, e.getMessage()
        ));
        writeToFile(outputFilename, String.format(
                "%d:%d %s", e.row, e.column, e.getMessage()
        ));
    }


    /**
     * Actions for the --parse option
     */
    void doParse(List<String> filenames) {
        List<XiSource> sources = XiSource.createMany(filenames);

        List<Tuple<Parsed, XiSource>> parsed = Lists.transform(sources,
            xs -> Tuple.of(Actions.parse(xs.reader), xs)
        );

        for (Tuple<Parsed, XiSource> p : parsed) {
            String outputFilename = diagPathOut(p.snd, "parsed");
            SExpOut sExpOut = new SExpOut(getFileOutputStream(outputFilename));

            Parsed result = p.fst;
            if (result.prog.isPresent()) {
                sExpOut.visit(result.prog.get());
                sExpOut.flush();
            } else {
                writeParseError(result.exception.get(), p.snd.filename, outputFilename);
            }
        }

        System.exit(0);
    }

    public void doTypecheck(List<String> filenames) {
        List<XiSource> sources = XiSource.createMany(filenames);

        List<Tuple<Parsed, XiSource>> parsedList = Lists.transform(sources,
            xs -> Tuple.of(Actions.parse(xs.reader), xs)
        );

        List<Either<FullProgram<Position>, XicException>> finList;
        finList = Lists.transform(parsedList, (Tuple<Parsed, XiSource> p) -> {
            Parsed parsed = p.fst;
            XiSource source = p.snd;

            if (!parsed.prog.isPresent()) {
                return Either.right(parsed.exception.get());
            } else {
                Program<Position> prog = parsed.prog.get();

                List<Tuple<Use<Position>, XiSource>> useFiles = Util.zip(
                    prog.uses,
                    XiSource.createMany(
                        libPath,
                        Lists.transform(prog.uses, u -> u.x.x + ".ixi")
                    )
                );

                List<Tuple<Use<Position>, Parsed>> parsedUseFiles = Lists.transform(useFiles,
                    u -> Tuple.of(u.fst, Actions.parseInterface(u.snd.reader))
                );

                Optional<XiUseException> useError = Optional.empty();
                List<Interface<Position>> interfaces = new ArrayList<>();
                for (Tuple<Use<Position>, Parsed> t : parsedUseFiles) {
                    if (t.snd.inter.isPresent()) {
                        interfaces.add(t.snd.inter.get());
                    } else {
                        useError = Optional.of(new XicException.XiUseException(
                            t.fst.x.x,
                            t.fst.a.row,
                            t.fst.a.col,
                            t.snd.exception.get().getMessage()
                        ));
                        break;
                    }
                }
                if (useError.isPresent()) {
                    return Either.right(useError.get());
                } else {
                    return Either.left(FullProgram.of(prog.a, prog, interfaces));
                }
            }
        });

        System.exit(0);
    }

    /**
     * Main entry point. Handles actions for the different CLI options.
     * @param args The command line arguments
     */
    private void doMain(String[] args) {

        try {
            parser.parseArgument(args);

            if (helpMode) {
                printUsage();
                System.exit(0);
            }

            if (arguments.isEmpty()) {
                System.out.println("No filenames provided.");
                printUsage();
                System.exit(1);
            }

            sourcePath = sourcePath.equals("") ?
                    "" : Files.simplifyPath(sourcePath);

            if (lexMode) {
                doLex(arguments);
            } else if (parseMode) {
                doParse(arguments);
            } else if (typecheckMode) {
                doTypecheck(arguments);
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
