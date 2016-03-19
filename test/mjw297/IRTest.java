package mjw297;

import com.google.common.collect.Lists;
import edu.cornell.cs.cs4120.xic.ir.interpret.IRSimulator;
import edu.cornell.cs.cs4120.xic.ir.parse.IRLexer;
import edu.cornell.cs.cs4120.xic.ir.parse.IRParser;
import junit.framework.Assert;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.*;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;
import edu.cornell.cs.cs4120.xic.ir.*;

import mjw297.Util.Tuple;

/**
 * Created by ralphrecto on 3/16/16.
 */
public class IRTest {

    private final List<String> testFileNames = new ArrayList<>();

    /* t -> JSexpOut sexp of FullProgram, where t\in testFileNames */
    private final Map<String, String> testFileNametoProg = new HashMap<>();

    /* t -> CompUnits generated from first IR gen pass */
    private final Map<String, IRCompUnit> irGenProgs = new HashMap<>();

    private final String baseTestDir = "examples/tests";
    Process irGen;

    public IRTest() {
        /* Add test file names */
        testFileNames.add("binopTests");

        testFileNames.forEach(fn -> {
            File f = Paths.get(baseTestDir, fn + ".xi").toFile();
            Parser p = null;
            Ast.Program<Position> prog = null;
            try {
                Actions.Parsed parsed = Actions.parse(new FileReader(f));
                if (!parsed.prog.isPresent()) {
                    throw parsed.exception.get();
                } else {
                    prog = parsed.prog.get();
                }
            } catch (Exception e) {
                System.out.println(e.getMessage());
                e.printStackTrace();
                System.exit(1);
            }

            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            SExpJaneStreetOut sexpOut = new SExpJaneStreetOut(baos);
            sexpOut.visit(Ast.FullProgram.of(prog.a, prog, new ArrayList<>()));
            sexpOut.flush();

            testFileNametoProg.put(fn, baos.toString());
        });


        List<String> args = new ArrayList<>();
        args.add("./bin/irGenTest.byte");
        args.addAll(testFileNametoProg.values());
        ProcessBuilder pb = new ProcessBuilder(args)
            .directory(
                Paths.get(System.getProperty("user.dir")).toFile()
            );

        try {
            irGen = pb.start();
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }

        BufferedReader brStdOut = new BufferedReader(
            new InputStreamReader(irGen.getInputStream())
        );
        BufferedReader brStdErr = new BufferedReader(
            new InputStreamReader(irGen.getErrorStream())
        );

        List<String> errors = brStdErr.lines().collect(Collectors.toList());
        errors.forEach(o -> System.out.println(o));

        if (errors.size() > 0) {
            System.exit(1);
        }

        Util.zip(testFileNames, brStdOut.lines().collect(Collectors.toList())).forEach(t -> {
            String testFileName = t.fst;
            String genIr = t.snd;

            IRParser p = new IRParser(new IRLexer(new StringReader(genIr)));
            try {
                irGenProgs.put(testFileName, (IRCompUnit) p.parse().value());
            } catch (Exception e) {
                e.printStackTrace();
                System.exit(1);
            }
        });
    }

    void longAssertTest(String filename, String testName, long expected) {
        IRSimulator sim = new IRSimulator(irGenProgs.get(filename));
        assertEquals(expected, sim.call(testName));
    }

    void printAssertTest(String filename, String testName, String expected) {
        IRSimulator sim = new IRSimulator(irGenProgs.get(filename));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(baos);
        PrintStream old = System.out;
        System.setOut(ps);
        sim.call(testName);
        System.out.flush();
        System.setOut(old);

        assertEquals(expected, baos.toString());
    }

    @Test
    public void basicBinopTest1() {
        longAssertTest("binopTests", "basicBinopTest1", 10l);
    }

    @Test
    public void basicBinopTest2() {
        longAssertTest("binopTests", "basicBinopTest2", 10l);
    }

    @Test
    public void basicBinopTest3() {
        longAssertTest("binopTests", "basicBinopTest3", 25l);
    }

    @Test
    public void basicBinopTest4() {
        longAssertTest("binopTests", "basicBinopTest4", 4l);
    }

    @Test
    public void basicBinopTest5() {
        longAssertTest("binopTests", "basicBinopTest5", 30l);
    }

    @Test
    public void basicBinopTest6() {
        longAssertTest("binopTests", "basicBinopTest6", 30l);
    }

    @Test
    public void stringTest1() {
        printAssertTest("binopTests", "stringTest1",
          "I have done it again."
        );
    }
}

