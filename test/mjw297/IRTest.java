package mjw297;

import edu.cornell.cs.cs4120.xic.ir.interpret.IRSimulator;
import edu.cornell.cs.cs4120.xic.ir.parse.IRLexer;
import edu.cornell.cs.cs4120.xic.ir.parse.IRParser;
import org.junit.Test;

import java.io.*;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import edu.cornell.cs.cs4120.xic.ir.*;

/**
 * Created by ralphrecto on 3/16/16.
 */
public class IRTest {

    Process irGen;

    public IRTest() {

        String xiProg = "test() : int { return 5; }";
        Actions.Parsed parsed = Actions.parse(new StringReader(xiProg));
        Ast.Program<Position> p = parsed.prog.get();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        SExpJaneStreetOut sexpOut = new SExpJaneStreetOut(baos);
        sexpOut.visit(Ast.FullProgram.of(p.a, p, new ArrayList<>()));
        sexpOut.flush();

        System.out.println(baos.toString());

        String workingDir = System.getProperty("user.dir");
        ProcessBuilder pb = new ProcessBuilder(
                "./bin/irGenTest.byte",
                baos.toString()
        ).directory(Paths.get(workingDir).toFile());

        try {
            irGen = pb.start();
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }

        InputStream is = irGen.getInputStream();
        InputStream os = irGen.getErrorStream();
        BufferedReader br = new BufferedReader(
            new InputStreamReader(is)
        );
        BufferedReader brOs = new BufferedReader(
                new InputStreamReader(os)
        );

        List<String> errors = brOs.lines().collect(Collectors.toList());
        errors.forEach(o -> System.out.println(o));

        if (errors.size() > 0) {
            System.exit(1);
        }

        List<String> outputs = br.lines().collect(Collectors.toList());
        for (String o : outputs) {
            System.out.println(o);
            IRParser parser = new IRParser(new IRLexer(new StringReader(o)));
            IRCompUnit compUnit = null;
            try {
                compUnit = parser.parse().value();
            } catch (Exception e) {
                e.printStackTrace();
            }
            IRSimulator sim = new IRSimulator(compUnit);
            System.out.println("result: " + sim.call("test"));
        }
    }

    @Test
    public void testyTest() {
        assert(true);
    }
}

