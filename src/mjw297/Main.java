package mjw297;
import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

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

    private void doLex() {
        if (arguments.isEmpty()) {
            System.out.println("No filenames provided.");
            printUsage();
        }

        List<FileReader> files = new ArrayList<>();
        for (String filename : arguments) {
            try {
                files.add(new FileReader(filename));
            } catch (FileNotFoundException e) {
                System.out.println("File " + filename + " does not exist.");
                return;
            }
        }

        for (FileReader file : files) {
            System.out.println("lexing file...");
        }
    }

    public void doMain(String[] args) {

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

    public static void main(String[] args) {
        new Main().doMain(args);
    }
}
