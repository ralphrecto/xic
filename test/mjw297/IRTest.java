package mjw297;

import org.junit.Test;

import java.io.*;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by ralphrecto on 3/16/16.
 */
public class IRTest {

    Process irGen;

    public IRTest() {

        String xiProg = "main(){ x:int = 5; }";
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

        List<String> outputs = br.lines().collect(Collectors.toList());
        outputs.forEach(o -> System.out.println(o));

        List<String> errors = brOs.lines().collect(Collectors.toList());
        errors.forEach(o -> System.out.println(o));
    }

    @Test
    public void testyTest() {
        assert(true);
    }
}

