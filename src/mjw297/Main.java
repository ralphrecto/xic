package mjw297;

import static com.google.common.collect.Iterables.any;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

import com.google.common.collect.Iterables;
import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

import com.google.common.collect.Lists;
import com.google.common.io.Files;

import java_cup.runtime.Symbol;

import lombok.AllArgsConstructor;

import mjw297.Actions.Lexed;
import mjw297.Actions.Parsed;
import mjw297.Ast.FullProgram;
import mjw297.Ast.Interface;
import mjw297.Ast.Program;
import mjw297.Ast.Use;
import mjw297.Main.XiSource;
import mjw297.Util.Either;
import mjw297.Util.Tuple;
import mjw297.XicException.ErrorType;
import mjw297.XicException.XiUseException;

/** The main compiler frontend/CLI interface to the compiler. */
public class Main {
    ////////////////////////////////////////////////////////////////////////////
    // flags
    ////////////////////////////////////////////////////////////////////////////
    /* Utility flags */
    @Option(name = "--help", usage = "Print a synopsis of options.")
    private static boolean helpMode = false;
    @Option(name="--report-opts", usage="Report available optimizations")
    private static boolean reportOpts = false;
    @Option(name = "-compilerpath", hidden = true, required = true)
    private static String compilerPath;
    @Option(name = "-O", usage = "Disable optimizations")
    private static boolean noOptimize = false;
    @Option(name = "-sourcepath", usage = "Specify where to find input source files")
    private static String sourcePath = "";
    @Option(name = "-libpath", usage = "Specify where to find input source files")
    private static String libPath = "";
    @Option(name = "-D", usage = "Specify where to place generated diagnostic files")
    private static String diagnosticPath = "";
    @Option(name = "-d", usage = "Specify where to place generated assembly files")
    private static String assemblyPath = "";
    @Option(name = "-target", usage = "Define target OS; only linux is a valid option. Defaults to linux")
    private static String targetOS = "linux";

    /* Compiler modes */
    @Option(name = "--lex", usage = "Generate output from lexical analysis; .lexed files")
    private static boolean lexMode = false;
    @Option(name = "--parse", usage = "Generate output from syntactic analysis; .parsed files")
    private static boolean parseMode = false;
    @Option(name = "--typecheck", usage = "Generate output from semantic analysis: .typed files")
    private static boolean typecheckMode = false;
    @Option(name = "--tcdebug", usage = "Generate debugging output for typechecking; .typeddebug files", hidden = true)
    private static boolean typecheckDebugMode = false;
    @Option(name = "--nolower", usage = "Generate non-lowered IR; .nolower files")
    private static boolean nolowerMode = false;
    @Option(name = "--lower", usage = "Lower IR; .lower files", hidden = true)
    private static boolean lowerMode = false;
    @Option(name = "--irgen", usage = "Generate intermediate code; .ir files")
    private static boolean irGenMode = false;
    @Option(name="--optir", usage="Optimized IR: .ir files")
    private static List<String> optIr;
    @Option(name="--optcfg", usage="Optimized CFG dot files: .dot files")
    private static List<String> optCfg;
    @Option(name = "--asmdebug", usage = "Asm gen debug mode: .s files", hidden = true)
    private static boolean asmDebugMode = false;

    /* Optimizations */
    @Option(name="-Oacf", usage="AST Constant folding")
    private static boolean astConstantFolding = false;
    @Option(name="-Oicf", usage="IR Constant folding")
    private static boolean irConstantFolding = false;
    @Option(name="-Oreg", usage="Register allocation")
    private static boolean registerAllocation = false;
    @Option(name="-Omc", usage="Move coalescing")
    private static boolean moveCoalescing = false;
    @Option(name="-Ouce", usage="Unreachable code elimination")
    private static boolean unreachableCodeElim = false;
    @Option(name="-Ocse", usage="Common subsexpression elimination")
    private static boolean commonSubexprElim = false;
    @Option(name="-Olicm", usage="Loop invariant code motion")
    private static boolean loopInvariantCodeMotion = false;
    @Option(name="-Opre", usage="Partial redundancy elimination")
    private static boolean partialRedundancyElim = false;
    @Option(name="-Ocp", usage="Constant propagation")
    private static boolean constantPropagation = false;
    @Option(name="-Ois", usage="Good instruction selection")
    private static boolean instructionSelection = false;

    @Option(name="-O-no-acf", usage="No AST constant folding")
    private static boolean noAstConstantFolding = false;
    @Option(name="-O-no-icf", usage="No IR constant folding")
    private static boolean noIrConstantFolding = false;
    @Option(name="-O-no-reg", usage="No register allocation")
    private static boolean noRegisterAllocation = false;
    @Option(name="-O-no-mc", usage="No move coalescing")
    private static boolean noMoveCoalescing = false;
    @Option(name="-O-no-uce", usage="No unreachable code elimination")
    private static boolean noUnreachableCodeElim = false;
    @Option(name="-O-no-cse", usage="No common subsexpression elimination")
    private static boolean noCommonSubexprElim = false;
    @Option(name="-O-no-licm", usage="No loop invariant code motion")
    private static boolean noLoopInvariantCodeMotion = false;
    @Option(name="-O-no-pre", usage="No partial redundancy elimination")
    private static boolean noPartialRedundancyElim = false;
    @Option(name="-O-no-cp", usage="No constant propagation")
    private static boolean noConstantPropagation = false;
    @Option(name="-O-no-is", usage="No good instruction selection")
    private static boolean noInstructionSelection = false;

    // some equivalences:
    // reg = mc
    // cp = uce
    // pre = cse = licm
    private static String acf  = "acf";
    private static String icf  = "icf";
    private static String reg  = "reg";
    private static String mc   = "mc";
    private static String uce  = "uce";
    private static String cse  = "cse";
    private static String licm = "licm";
    private static String pre  = "pre";
    private static String cp   = "cp";
    private static String is   = "is";

    private static String acfFlag  = "--" + acf;
    private static String icfFlag  = "--" + icf;
    private static String regFlag  = "--" + reg;
    private static String mcFlag   = "--" + reg;
    private static String uceFlag  = "--" + cp;
    private static String cseFlag  = "--" + pre;
    private static String licmFlag = "--" + pre;
    private static String preFlag  = "--" + pre;
    private static String cpFlag   = "--" + cp;
    private static String isFlag   = "--" + is;

    @Argument(usage = "Other non-optional arguments.", hidden = true)
    private static List<String> arguments = new ArrayList<String>();

    private CmdLineParser parser;

    public Main() {
        this.parser = new CmdLineParser(this);
    }

    ////////////////////////////////////////////////////////////////////////////
    // XiSource
    ////////////////////////////////////////////////////////////////////////////
    /**
     * {@code XiSource} represents a Xi Source file. Any instance of this
     * necessarily has a .xi or .ixi extension.
     */
    @AllArgsConstructor
    static class XiSource {
        public String filename;
        public File file;
        public FileReader reader;

        /**
         * Change the source's extension. Use for output files that will live in
         * the same directory. It is a precondition that the file extension is
         * ".xi".
         *
         * changeExtension(foo/bar/baz.xi, yolo) -> foo/bar/baz.yolo
         */
        String changeExtension(String newExt) {
            assert filename.substring(filename.length() - 3, filename.length()).equals(".xi");
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
                File f = Paths.get(filename).isAbsolute() ?
                    Paths.get(filename).toFile() :
                    Paths.get(baseDir, filename).toFile();
                return new XiSource(filename, f, new FileReader(f));
            } catch (FileNotFoundException e) {
                e.printStackTrace();
                System.exit(1);
                return null;
            }
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

    ////////////////////////////////////////////////////////////////////////////
    // helpers
    ////////////////////////////////////////////////////////////////////////////
    /**
     * Helper function to print binary usage info
     */
    private void printUsage() {
        System.out.println("xic [options] <source files>");
        parser.printUsage(System.err);
    }

    private String diagPathOut(XiSource xs, String ext) {
        String outPath = assemblyPath.equals("") ?
            diagnosticPath : assemblyPath;
        return Paths.get(outPath, xs.changeExtension(ext))
            .toAbsolutePath()
            .toString();
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

    private void printError(String kind, String filename,
                            String line, String col, String msg) {
        System.out.println(String.format(
            "%s error at %s:%s:%s: %s",
            kind, filename, line, col, msg
        ));
    }

    private void printError(String kind, String filename,
                            int line, int col, String msg) {
        printError(kind, filename, "" + line, "" + col, msg);
    }

    private void writeParseError(XicException e, String filename, String outputFilename) {
        String kind = e.type == ErrorType.LEXING ? "Lexical" : "Syntactic";
        printError(
            kind, filename, e.row, e.column, e.getMessage()
        );
        writeToFile(outputFilename, String.format(
            "%d:%d %s", e.row, e.column, e.getMessage()
        ));
    }

    private Interface<Position> parseInterface(Use<Position> use) throws XicException {
      String interName = use.x.x + ".ixi";
      XiSource interfaceFile = XiSource.create(libPath, interName);
      Parsed parsed = Actions.parseInterface(interfaceFile.reader);

      if (parsed.inter.isPresent()) {
        return parsed.inter.get();
      } else {
        throw new XicException.XiUseException(
          interName, -1, -1,
          parsed.exception.get().getMessage()
        );
      }
    }

    private List<Interface<Position>> parseInterfaces(List<Use<Position>> uses) throws XicException {
        List<Interface<Position>> interfaces = new ArrayList<>();
        for (Use<Position> use : uses) {
            interfaces.add(parseInterface(use));
        }
        return interfaces;
    }

    /* Perform a BFS through uses of interface files. Take initial uses from program. */
    private List<Interface<Position>> useTraverse(List<Use<Position>> initUses) throws XicException {
        List<Tuple<String, Interface<Position>>> init = Util.zip(
            Lists.transform(initUses, u -> u.x.x),
            parseInterfaces(initUses)
        );
        Queue<Tuple<String, Interface<Position>>> queue =
            (Queue<Tuple<String, Interface<Position>>>) new LinkedList<> (init);
        List<Interface<Position>> retList = new ArrayList<>();
        Set<String> visited = new HashSet<>();

        Tuple<String, Interface<Position>> current = null;
        while (!queue.isEmpty()) {
            current = queue.poll();
            visited.add(current.fst);
            retList.add(current.snd);
            for (Use<Position> use : current.snd.uses) {
                if (!visited.contains(use.x.x)) {
                    queue.add(Tuple.of(use.x.x, parseInterface(use)));
                }
            }
        }
        return retList;
    }

    private Tuple<
                List<Tuple<XiSource, XicException>>,
                List<Tuple<XiSource, FullProgram<Position>>>
            > fullParse(List<String> filenames) {

        List<XiSource> sources = XiSource.createMany(filenames);

        List<Tuple<Parsed, XiSource>> parsedList = Lists.transform(sources,
            xs -> Tuple.of(Actions.parse(xs.reader), xs)
        );

        List<Tuple<XiSource, Either<FullProgram<Position>, XicException>>> resultList;
        resultList = Lists.transform(parsedList, (Tuple<Parsed, XiSource> p) -> {
            Parsed parsed = p.fst;
            XiSource source = p.snd;

            if (!parsed.prog.isPresent()) {
                return Tuple.of(source, Either.right(parsed.exception.get()));
            } else {
                Program<Position> prog = parsed.prog.get();
                try {
                    List<Interface<Position>> interfaces = useTraverse(prog.uses);
                    return Tuple.of(
                        source,
                        Either.left(FullProgram.of(prog.a, source.filename, prog, interfaces))
                    );
                } catch (XicException e) {
                    return Tuple.of(source, Either.right(e));
                }
            }
        });

        List<Tuple<XiSource, XicException>> errors = new ArrayList<>();
        List<Tuple<XiSource, FullProgram<Position>>> programs = new ArrayList<>();

        for (Tuple<XiSource, Either<FullProgram<Position>, XicException>> result : resultList) {
            if (result.snd.isLeft()) {
                programs.add(Tuple.of(result.fst, result.snd.getLeft()));
            } else {
                errors.add(Tuple.of(result.fst, result.snd.getRight()));
            }
        }

        return Tuple.of(errors, programs);
    }

    /**
     * callOCaml(files, flags, ext) passes files and flags to the OCaml
     * frontend which in turn writes its own files with the ext extension.
     *  returns List<Tuple<source, stdout>> */
    public void callOCaml(List<String> filenames, List<String> binArgs,
                          String extension) {
        Tuple<
            List<Tuple<XiSource, XicException>>,
            List<Tuple<XiSource, FullProgram<Position>>>
        > fullyParsed = fullParse(filenames);

        List<Tuple<XiSource, XicException>> errors = fullyParsed.fst;
        errors.forEach(t -> {
            writeParseError(t.snd, t.fst.filename, diagPathOut(t.fst, extension));
        });

        List<Tuple<XiSource, FullProgram<Position>>> programs = fullyParsed.snd;

        if (programs.size() > 0) {
          binArgs.add("--astfiles");
          programs.forEach(t -> {
              XiSource src = t.fst;
              FullProgram<Position> prog = t.snd;

              // Write file
              String outputFilename = diagPathOut(src, extension);
              File outputFile = Paths.get(outputFilename).toFile();
              try {
                  SExpJaneStreetOut sexpOut
                      = new SExpJaneStreetOut(new FileOutputStream(outputFile));
                  sexpOut.visit(prog);
                  sexpOut.flush();
              } catch(IOException e) {
                System.out.println(e.getMessage());
                e.printStackTrace();
                System.exit(1);
              }

              // Pass filename to OCaml
              binArgs.add(outputFilename);
          });
        }

        List<String> args = new ArrayList<>();
        args.add("./bin/main.byte");
        args.addAll(binArgs);

        ProcessBuilder pb = new ProcessBuilder(args)
            .directory(Paths.get(compilerPath).toFile())
            .redirectOutput(ProcessBuilder.Redirect.INHERIT);
        Process proc = null;

        try {
            proc = pb.start();
            proc.waitFor();
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        BufferedReader stdErr = new BufferedReader(
            new InputStreamReader(proc.getErrorStream())
        );
        List<String> stdErrors = stdErr.lines().collect(Collectors.toList());
        stdErrors.forEach(System.out::println);

        BufferedReader stdOut = new BufferedReader(
            new InputStreamReader(proc.getInputStream())
        );
        List<String> stdOuts = stdOut.lines().collect(Collectors.toList());
        stdOuts.forEach(System.out::println);
    }

    private static <A> List<A> dedup(List<A> xs) {
        // http://stackoverflow.com/a/2849570/3187068
        Set<A> nodups = new HashSet<>();
        nodups.addAll(xs);
        return new ArrayList<>(nodups);
    }

    /**
     * Given a mix of -O<opt>, -O, and -O-no-<opt>, figure out which
     * optimizations should actually be set. See the README to see a full case
     * analysis of which sets of flags are permitted and what they do.
     */
    private List<String> gatherOpts() {
        List<Boolean> opts = Arrays.asList(
            astConstantFolding,
            irConstantFolding,
            registerAllocation,
            moveCoalescing,
            unreachableCodeElim,
            commonSubexprElim,
            loopInvariantCodeMotion,
            partialRedundancyElim,
            constantPropagation,
            instructionSelection
        );
        boolean optsSpecified = any(opts, b -> b);

        List<Boolean> noOpts = Arrays.asList(
            noIrConstantFolding,
            noAstConstantFolding,
            noRegisterAllocation,
            noMoveCoalescing,
            noUnreachableCodeElim,
            noCommonSubexprElim,
            noLoopInvariantCodeMotion,
            noPartialRedundancyElim,
            noConstantPropagation,
            noInstructionSelection
        );
        boolean noOptsSpecified = any(noOpts, b -> b);

        if (optsSpecified && noOptsSpecified) {
            System.out.println("-O<opt> and -O-no-<opt> are mutually exclusive");
            System.exit(1);
        }

        // -O is only effective when neither of -O<opt>, -O-no-<opt> specified
        if (noOptimize && !optsSpecified && !noOptsSpecified) {
            return new ArrayList<String>();
        }

        // if nothing is specified, we perform all optimizations
        List<String> allFlags = dedup(new ArrayList<>(Arrays.asList(
            acfFlag, icfFlag, regFlag, mcFlag, uceFlag, cseFlag, licmFlag,
            preFlag, cpFlag, isFlag
        )));
        if (!optsSpecified && !noOptsSpecified) {
            return dedup(allFlags);
        }

        // -O-<opt>
        if (optsSpecified) {
            List<String> flags = new ArrayList<>();
            if (astConstantFolding)      { flags.add(acfFlag);  }
            if (irConstantFolding)       { flags.add(icfFlag);  }
            if (registerAllocation)      { flags.add(regFlag);  }
            if (moveCoalescing)          { flags.add(mcFlag);   }
            if (unreachableCodeElim)     { flags.add(uceFlag);  }
            if (commonSubexprElim)       { flags.add(cseFlag);  }
            if (loopInvariantCodeMotion) { flags.add(licmFlag); }
            if (partialRedundancyElim)   { flags.add(preFlag);  }
            if (constantPropagation)     { flags.add(cpFlag);   }
            if (instructionSelection)    { flags.add(isFlag);   }
            return dedup(flags);
        }

        // -O-no-<opt>
        assert noOptsSpecified;
        List<String> flags = allFlags;
        if (noAstConstantFolding)      { flags.remove(acfFlag);  }
        if (noIrConstantFolding)       { flags.remove(icfFlag);  }
        if (noRegisterAllocation)      { flags.remove(regFlag);  }
        if (noMoveCoalescing)          { flags.remove(mcFlag);   }
        if (noUnreachableCodeElim)     { flags.remove(uceFlag);  }
        if (noCommonSubexprElim)       { flags.remove(cseFlag);  }
        if (noLoopInvariantCodeMotion) { flags.remove(licmFlag); }
        if (noPartialRedundancyElim)   { flags.remove(preFlag);  }
        if (noConstantPropagation)     { flags.remove(cpFlag);   }
        if (instructionSelection)      { flags.remove(isFlag);   }
        return dedup(flags);
    }

    ////////////////////////////////////////////////////////////////////////////
    // modes
    ////////////////////////////////////////////////////////////////////////////
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
                printError("Lexical",
                    t.snd.filename, e.row, e.column, e.getMessage()
                );
                outputBuilder.append(
                    String.format("%d:%d %s\n", e.row, e.column, e.getMessage())
                );
            }

            String outputFilename = diagPathOut(t.snd, "lexed");
            writeToFile(outputFilename, outputBuilder.toString());
        }
    }

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

    void doMode(String mode, String ext, List<String> files, List<String> opts) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add(mode);
        binArgs.addAll(opts);
        callOCaml(files, binArgs, ext);
    }

    void doTypecheck(List<String> filenames, List<String> opts) {
        doMode("--typecheck", "typed", filenames, opts);
    }

    void doTypecheckDebug(List<String> filenames, List<String> opts) {
        doMode("--tcdebug", "typeddebug", filenames, opts);
    }

    void doIrNolower(List<String> filenames, List<String> opts) {
        doMode("--nolower", "nolower", filenames, opts);
    }

    void doIRLower(List<String> filenames, List<String> opts) {
        doMode("--lower", "lower", filenames, opts);
    }

    void doIRGen(List<String> filenames, List<String> opts) {
        // Unlike most other flags, --irgen restricts the set of optimizations
        // that are meant to be performed. Specifically, --irgen only performs
        // constant folding. See the README for more information.
        List<String> filtered = new ArrayList<>();
        if (opts.contains(acfFlag)) {
            filtered.add(acfFlag);
        }
        if (opts.contains(icfFlag)) {
            filtered.add(icfFlag);
        }
        doMode("--irgen", "ir", filenames, filtered);
    }

    void doOpt(String prefix, String ext, List<String> phases,
               List<String> filenames, List<String> opts) {
        if (any(phases, s -> !(s.equals("initial") || s.equals("final")))) {
            System.out.printf("invalid --%s argument\n", prefix);
            System.exit(-1);
        }

        List<String> newOpts = new ArrayList<>();
        if (phases.contains("initial")) {
            newOpts.add(String.format("--%s-initial", prefix));
        }
        if (phases.contains("final")) {
            newOpts.add(String.format("--%s-final", prefix));
        }
        newOpts.addAll(opts);
        callOCaml(filenames, newOpts, ext);
    }

    void doOptIr(List<String> filenames, List<String> opts) {
        doOpt("optir", "optir", optIr, filenames, opts);
    }

    void doOptCfg(List<String> filenames, List<String> opts) {
        doOpt("optcfg", "dot", optCfg, filenames, opts);
    }

    void doAsmGenDebug(List<String> filenames, List<String> opts) {
        doMode("--asmdebug", "s", filenames, opts);
    }

    void doAsmGen(List<String> filenames, List<String> opts) {
        callOCaml(filenames, opts, "s");
    }

    ////////////////////////////////////////////////////////////////////////////
    // main
    ////////////////////////////////////////////////////////////////////////////
    private void doMain(String[] args) {
        try {
            parser.parseArgument(args);

            if (helpMode) {
                printUsage();
                System.exit(0);
            }


            if (reportOpts) {
                List<String> opts = Arrays.asList(
                    acf, icf, reg, mc, uce, cse, licm, pre, cp, is
                );
                opts.forEach(System.out::println);
                System.exit(0);
            }

            if (arguments.isEmpty()) {
                System.out.println("No filenames provided.");
                printUsage();
                System.exit(1);
            }

            if (!targetOS.equals("linux")) {
                System.out.println("-target: linux is currently the only supported target");
                System.exit(1);
            }

            sourcePath = sourcePath.equals("") ?
                    "" : Files.simplifyPath(sourcePath);
            List<String> opts = gatherOpts();

                 if (lexMode)            { doLex           (arguments); }
            else if (parseMode)          { doParse         (arguments); }
            else if (typecheckMode)      { doTypecheck     (arguments, opts); }
            else if (typecheckDebugMode) { doTypecheckDebug(arguments, opts); }
            else if (nolowerMode)        { doIrNolower     (arguments, opts); }
            else if (lowerMode)          { doIRLower       (arguments, opts); }
            else if (irGenMode)          { doIRGen         (arguments, opts); }
            else if (optIr != null)      { doOptIr         (arguments, opts); }
            else if (optCfg != null)     { doOptCfg        (arguments, opts); }
            else if (asmDebugMode)       { doAsmGenDebug   (arguments, opts); }
            else                         { doAsmGen        (arguments, opts); }
        } catch(CmdLineException e) {
            System.err.println(e.getMessage());
            printUsage();
        }
    }

    public static void main(String[] args) {
        new Main().doMain(args);
    }
}
