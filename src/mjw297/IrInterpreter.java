package mjw297;

import java.io.FileReader;

import edu.cornell.cs.cs4120.xic.ir.IRCompUnit;
import edu.cornell.cs.cs4120.xic.ir.interpret.IRSimulator;
import edu.cornell.cs.cs4120.xic.ir.parse.IRLexer;
import edu.cornell.cs.cs4120.xic.ir.parse.IRParser;

public class IrInterpreter {
    public static void main(String[] args) throws Exception {
        if (args.length != 1)  {
            System.err.println("usage: ./ir ir_file");
            System.exit(-1);
        }

        String filename = args[0];
        IRLexer lexer = new IRLexer(new FileReader(filename));
        IRParser parser = new IRParser(lexer);
        IRSimulator sim = new IRSimulator((IRCompUnit) parser.parse().value);
        sim.call("_Imain_paai", 0l);
    }
}
