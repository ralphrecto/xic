package mjw297;
import java_cup.runtime.Symbol;
import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;
import com.google.common.io.Files;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class Main {

    @Option(name="--help",usage="Print a synopsis of options.")
    private boolean helpMode = false;

    @Option(name="--lex",usage="Generate output from lexical analysis")
    private boolean lexMode = false;

    @Argument(usage="Other non-optional arguments.", hidden=true)
    private static List<String> arguments = new ArrayList<String>();

    private CmdLineParser parser;

    public Main() {
        this.parser = new CmdLineParser(this);
    }

    private void printUsage() {
        System.err.println("xic [options] <source files>");
        parser.printUsage(System.err);
    }

    private void doLex() throws XicException {
        if (arguments.isEmpty()) {
            System.out.println("No filenames provided.");
            printUsage();
        }

        class SourceFile {
            String filename;
            FileReader reader;

            SourceFile(String filename, FileReader reader) {
                this.filename = filename;
                this.reader = reader;
            }
        }

        List<SourceFile> files = new ArrayList<>();
        for (String filename : arguments) {
            if (!Files.getFileExtension(filename).equals("xi")) {
                System.out.println("Valid Xi files must have a .xi extension.");
                return;
            }

            try {
                files.add(new SourceFile(filename, new FileReader(filename)));
            } catch (FileNotFoundException e) {
                System.out.println("File " + filename + " does not exist.");
                return;
            }
        }

        /* lex each file and output the generated tokens */
        for (SourceFile sf : files) {
            Lexer lexer = new Lexer(sf.reader);
            StringBuilder outputBuilder = new StringBuilder();

            Symbol curSym;
            try {
                curSym = lexer.next_token();
                while (curSym.sym != Sym.EOF) {
                    outputBuilder.append(
                            String.format(
                                    "%d:%d %s\n",
                                    curSym.left,
                                    curSym.right,
                                    SymUtil.symToLiteral(curSym)
                            )
                    );
                    curSym = lexer.next_token();
                }
            } catch (IOException e) {
                e.printStackTrace();
                return;
            }

            String outputFilename = String.format(
                    "%s.lexed",
                    Files.getNameWithoutExtension(sf.filename)
            );
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

    public void doMain(String[] args) throws XicException {

        try {
            parser.parseArgument(args);
            if (helpMode) {
                printUsage();
            } else if (lexMode) {
                doLex();
            } else {
                if (arguments.isEmpty()) {
                    printUsage();
                }
            }
        } catch(CmdLineException e) {
            System.err.println(e.getMessage());
            printUsage();
        }

        return;
    }

    public static void main(String[] args) throws XicException {
        new Main().doMain(args);
    }
}
