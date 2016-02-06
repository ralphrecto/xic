package mjw297;
import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

import java.util.ArrayList;
import java.util.List;

public class Main {
    @Option(name="--help",usage="Print a synopsis of options.")
    public boolean helpMode = false;

    @Option(name="--lex",usage="Generate output from lexical analysis")
    public boolean lexMode = false;

    /* other non optional arguments (i.e. filenames) */
    @Argument
    private static List<String> arguments = new ArrayList<String>();

    public void doMain(String[] args) {
        CmdLineParser parser = new CmdLineParser(this);

        Runnable printUsage = () -> {
            System.err.println("xic [options] <source files>");
            parser.printUsage(System.err);
        };

        try {
            parser.parseArgument(args);

            if (helpMode) {
                printUsage.run();
            } else if (lexMode) {
                System.out.println("fLEXing");
            } else if(arguments.isEmpty()) {
                System.out.println("no arguments given.");
                printUsage.run();
            }

        } catch( CmdLineException e ) {
            System.err.println(e.getMessage());
            printUsage.run();
        }

        return;
    }

    public static void main(String[] args) {
        new Main().doMain(args);
    }
}
