package mjw297;
import mjw297.XicException.*;
import com.google.common.collect.Lists;
import java_cup.runtime.Symbol;
import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;
import com.google.common.io.Files;

import java.io.*;
import java.nio.file.Paths;
import java.nio.charset.Charset;
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

        static XiSource create(String filename) throws XiSourceException {
            if (!Files.getFileExtension(filename).equals("xi")) {
                throw new XiSourceException("Valid Xi files must have .xi extension");
            }
            try {
                File f = Paths.get(sourcePath, filename).toFile();
                return new XiSource(
                        filename,
                        f,
                        new FileReader(f)
                );
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

    /**
     * Actions for the --lex option
     */
    private void lexOut(List<Util.Tuple<Actions.Lexed, XiSource>> lexedOut) {
        for (Util.Tuple<Actions.Lexed, XiSource> t : lexedOut) {

            Actions.Lexed lexed = t.fst;

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
                outputBuilder.append(
                    String.format("%d:%d %s\n", e.row, e.column, e.getMessage())
                );
            }

            String outputFilename = diagPathOut(t.snd, "lexed");
            File outputFile = Paths.get(outputFilename).toFile();

            try {
                Files.write(outputBuilder.toString().getBytes(), outputFile);
            } catch (IOException e) {
                e.printStackTrace();
                System.out.println("Could not write to file " + outputFilename);
                System.exit(1);
            }
        }
    }

    /**
     * Actions for the --parse option
     */
    void parseOut(List<Util.Tuple<Actions.Parsed, XiSource>> parsed) {
        for (Util.Tuple<Actions.Parsed, XiSource> p : parsed) {

            File outputFile = Paths.get(diagPathOut(p.snd, "parsed")).toFile();

            SExpJaneStreetOut sExpOut = null;
            try {
                sExpOut = new SExpJaneStreetOut(new FileOutputStream(outputFile));
            } catch (FileNotFoundException e) {
                e.printStackTrace();
                System.exit(1);
            }

            Actions.Parsed result = p.fst;
            if (result.prog.isPresent()) {
                sExpOut.visit(result.prog.get());
                sExpOut.flush();
            } else {
                try {
                    Files.write(
                        result.exception.get().getMessage().toString().getBytes(),
                        outputFile
                    );
                } catch (IOException e) {
                    e.printStackTrace();
                    System.exit(1);
                }
            }
        }

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

            Staging staging = new Staging(arguments);

            if (lexMode) {
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
