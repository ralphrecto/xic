package mjw297;

import org.junit.Test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by ralphrecto on 3/16/16.
 */
public class IRTest {

    Process irGen;

    public IRTest() {
        String workingDir = System.getProperty("user.dir");
        ProcessBuilder pb = new ProcessBuilder(
                "./bin/irGenTest.byte",
                "poop",
                "poopy"
        )
            .directory(Paths.get(workingDir).toFile());
        try {
            irGen = pb.start();
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }

        InputStream is = irGen.getInputStream();
        BufferedReader br = new BufferedReader(
            new InputStreamReader(is)
        );

        List<String> outputs = br.lines().collect(Collectors.toList());
        outputs.forEach(o -> System.out.println(o));
    }

    @Test
    public void testyTest() {
        assert(true);
    }
}

