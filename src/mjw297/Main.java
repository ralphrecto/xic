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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

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
    /* Utility flags */
    @Option(name = "--help", usage = "Print a synopsis of options.")
    private static boolean helpMode = false;

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

    @Option(name = "--typecheck", usage = "Generate output from semantic analysis")
    private static boolean typecheckMode = false;

    @Option(name = "--tcdebug", usage = "Generate debugging output for typechecking; .typeddebug files", hidden = true)
    private static boolean typecheckDebugMode = false;

    @Option(name = "--irgen", usage = "Generate intermediate code; .ir files")
    private static boolean irGenMode = false;

    @Option(name = "--ast-cfold", usage = "Constant fold on AST; .astcfold files", hidden = true)
    private static boolean astCfoldMode = false;

    @Option(name = "--ir-acfold", usage = "Constant fold on AST, generate ir; .iracfold files", hidden = true)
    private static boolean irAstCfoldMode = false;

    @Option(name = "--ir-cfold", usage = "Constant fold on IR; .ircfold files", hidden = true)
    private static boolean irCfoldMode = false;

    @Option(name = "--lower", usage = "Lower IR; .lower files", hidden = true)
    private static boolean lowerMode = false;

    @Option(name = "--blkreorder", usage = "Reorder blocks; .blkreorder files", hidden = true)
    private static boolean blkReorderMode = false;

    @Option(name = "--basicir",
        usage = "IR generation with no opt, lowering or blk reorder; .basicir files",
        hidden = true)
    private static boolean basicIRMode = false;

    @Option(name = "--asmchomp", usage = "Use chomp instead of munch; .s files", hidden = true)
    private static boolean asmChompMode = false;

    @Option(name = "--asmdebug", usage = "Asm gen debug mode", hidden = true)
    private static boolean asmDebugMode = false;

    /* Optimizations */
    @Option(name="--report-opts", usage="Report available optimizations")
    private static boolean reportOpts = false;

    @Option(name="--optir", usage="Optimized IR")
    private static String optIr = "";

    @Option(name="--optcfg", usage="Optimized CFG dot files")
    private static String optCfg = "";

    @Option(name="-Ocf", usage="Constant folding")
    private static boolean constantFolding = false;
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

    @Option(name="-O-no-cf", usage="No constant folding")
    private static boolean noConstantFolding = false;
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

    // some equivalences:
    // reg = mc
    // cp = uce
    // pre = cse = licm
    private static String cf   = "cf";
    private static String reg  = "reg";
    private static String mc   = "mc";
    private static String uce  = "uce";
    private static String cse  = "cse";
    private static String licm = "licm";
    private static String pre  = "pre";
    private static String cp   = "cp";

    private static String cfFlag   = "--" + cf;
    private static String regFlag  = "--" + reg;
    private static String mcFlag   = "--" + reg;
    private static String uceFlag  = "--" + cp;
    private static String cseFlag  = "--" + pre;
    private static String licmFlag = "--" + pre;
    private static String preFlag  = "--" + pre;
    private static String cpFlag   = "--" + cp;

    @Argument(usage = "Other non-optional arguments.", hidden = true)
    private static List<String> arguments = new ArrayList<String>();

    private CmdLineParser parser;

    public Main() {
        this.parser = new CmdLineParser(this);
    }

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

    private void printError(String kind, String filename, String line, String col, String msg) {
        System.out.println(String.format(
            "%s error at %s:%s:%s: %s",
            kind, filename, line, col, msg
        ));
    }

    private void printError(String kind, String filename, int line, int col, String msg) {
        printError(kind, filename, "" + line, "" + col, msg);
    }

    /**
     * Actions for the --lex option
     */
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

    void writeParseError(XicException e, String filename, String outputFilename) {
        String kind = e.type == ErrorType.LEXING ? "Lexical" : "Syntactic";
        printError(
            kind, filename, e.row, e.column, e.getMessage()
        );
        writeToFile(outputFilename, String.format(
            "%d:%d %s", e.row, e.column, e.getMessage()
        ));
    }


    /**
     * Actions for the --parse option
     */
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

    private Tuple<List<Tuple<XiSource, XicException>>,List<Tuple<XiSource, FullProgram<Position>>>>
            fullParse(List<String> filenames) {

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

                List<Tuple<Use<Position>, XiSource>> useFiles = Util.zip(
                    prog.uses,
                    XiSource.createMany(
                        libPath,
                        Lists.transform(prog.uses, u -> u.x.x + ".ixi")
                    )
                );

                List<Tuple<Use<Position>, Parsed>> parsedUseFiles = Lists.transform(useFiles,
                    u -> Tuple.of(u.fst, Actions.parseInterface(u.snd.reader))
                );

                Optional<XiUseException> useError = Optional.empty();
                List<Interface<Position>> interfaces = new ArrayList<>();
                for (Tuple<Use<Position>, Parsed> t : parsedUseFiles) {
                    if (t.snd.inter.isPresent()) {
                        interfaces.add(t.snd.inter.get());
                    } else {
                        useError = Optional.of(new XicException.XiUseException(
                            t.fst.x.x,
                            t.fst.a.row,
                            t.fst.a.col,
                            t.snd.exception.get().getMessage()
                        ));
                        break;
                    }
                }
                if (useError.isPresent()) {
                    return Tuple.of(source, Either.right(useError.get()));
                } else {
                    return Tuple.of(
                        source,
                        Either.left(FullProgram.of(prog.a, source.filename, prog, interfaces))
                    );
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

    /* binArgs are additional options to pass to the OCaml binary
     *  returns List<Tuple<source, stdout>> */
    public void callOCaml(List<String> filenames, List<String> binArgs, String extension) {
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

              // Convert AST to sexp
              ByteArrayOutputStream baos = new ByteArrayOutputStream();
              SExpJaneStreetOut sexpOut = new SExpJaneStreetOut(baos);
              sexpOut.visit(prog);
              sexpOut.flush();

              // Write file
              String outputFilename = diagPathOut(src, extension);
              File outputFile = Paths.get(outputFilename).toFile();
              try {
                Files.write(baos.toString().getBytes(), outputFile);
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
            .directory(Paths.get(compilerPath).toFile());
        Process proc = null;

        try {
            proc = pb.start();
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
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

    void doTypecheck(List<String> filenames) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add("--typecheck");

        callOCaml(filenames, binArgs, "typed");
    }

    void doTypecheckDebug(List<String> filenames) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add("--tcdebug");

        callOCaml(filenames, binArgs, "typeddebug");
    }

    void doAstCfold(List<String> filenames) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add("--ast-cfold");

        callOCaml(filenames, binArgs, "astcfold");
    }

    void doIRAstCfold(List<String> filenames) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add("--ir-acfold");

        callOCaml(filenames, binArgs, "iracfold");
    }

    void doIRCfold(List<String> filenames) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add("--ir-cfold");

        callOCaml(filenames, binArgs, "ircfold");
    }

    void doIRLower(List<String> filenames) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add("--lower");

        callOCaml(filenames, binArgs, "lower");
    }

    void doIRBlkReorder(List<String> filenames) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add("--blkreorder");

        callOCaml(filenames, binArgs, "blkreorder");
    }

    void doIRGenNoOpt(List<String> filenames) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add("--irgen");
        binArgs.add("--no-opt");

        callOCaml(filenames, binArgs, "ir");
    }

    void doIRGen(List<String> filenames) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add("--irgen");

        callOCaml(filenames, binArgs, "ir");
    }

    void doBasicIR(List<String> filenames) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add("--basicir");

        callOCaml(filenames, binArgs, "basicir");
    }

    void doAsmGenNoOpt(List<String> filenames) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add("--no-opt");
        binArgs.add("--asmchomp");

        if (asmDebugMode) {
            binArgs.add("--asmdebug");
        }

        callOCaml(filenames, binArgs, "s");
    }

    void doAsmGen(List<String> filenames) {
        List<String> binArgs = new ArrayList<>();
        binArgs.add("--asmchomp");

        if (asmDebugMode) {
            binArgs.add("--asmdebug");
        }

        callOCaml(filenames, binArgs, "s");
    }

    /**
     * Given a mix of -O<opt>, -O, and -O-no-<opt>, figure out which
     * optimizations should actually be set. If both -O and -O-no-opt flags are
     * set, we die with error.
     */
    public List<String> gatherOpts() {
        List<Boolean> opts = Arrays.asList(
            constantFolding,
            registerAllocation,
            moveCoalescing,
            unreachableCodeElim,
            commonSubexprElim,
            loopInvariantCodeMotion,
            partialRedundancyElim,
            constantPropagation
        );
        boolean optsSpecified = any(opts, b -> b);

        List<Boolean> noOpts = Arrays.asList(
            noConstantFolding,
            noRegisterAllocation,
            noMoveCoalescing,
            noUnreachableCodeElim,
            noCommonSubexprElim,
            noLoopInvariantCodeMotion,
            noPartialRedundancyElim,
            noConstantPropagation
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
        List<String> allFlags = new ArrayList<>(Arrays.asList(
            cfFlag, regFlag, mcFlag, uceFlag, cseFlag, licmFlag, preFlag, cpFlag
        ));
        if (!optsSpecified && !noOptsSpecified) {
            return allFlags;
        }

        // -O-<opt>
        if (optsSpecified) {
            List<String> flags = new ArrayList<>();
            if (constantFolding)         { flags.add(cfFlag);   }
            if (registerAllocation)      { flags.add(regFlag);  }
            if (moveCoalescing)          { flags.add(mcFlag);   }
            if (unreachableCodeElim)     { flags.add(uceFlag);  }
            if (commonSubexprElim)       { flags.add(cseFlag);  }
            if (loopInvariantCodeMotion) { flags.add(licmFlag); }
            if (partialRedundancyElim)   { flags.add(preFlag);  }
            if (constantPropagation)     { flags.add(cpFlag);   }
            return flags;
        }

        // -O-no-<opt>
        assert noOptsSpecified;
        List<String> flags = allFlags;
        if (noConstantFolding)         { flags.remove(cfFlag);   }
        if (noRegisterAllocation)      { flags.remove(regFlag);  }
        if (noMoveCoalescing)          { flags.remove(mcFlag);   }
        if (noUnreachableCodeElim)     { flags.remove(uceFlag);  }
        if (noCommonSubexprElim)       { flags.remove(cseFlag);  }
        if (noLoopInvariantCodeMotion) { flags.remove(licmFlag); }
        if (noPartialRedundancyElim)   { flags.remove(preFlag);  }
        if (noConstantPropagation)     { flags.remove(cpFlag);   }
        return flags;
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


            if (reportOpts) {
                List<String> opts = Arrays.asList(
                    cf, reg, mc, uce, cse, licm, pre, cp
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

            // TODO: pass opts around
            List<String> opts = gatherOpts();

            if (lexMode) {
                doLex(arguments);
            } else if (parseMode) {
                doParse(arguments);
            } else if (typecheckMode) {
                doTypecheck(arguments);
            } else if (typecheckDebugMode) {
                doTypecheckDebug(arguments);
            } else if (astCfoldMode) {
                doAstCfold(arguments);
            } else if (irGenMode && noOptimize) {
                doIRGenNoOpt(arguments);
            } else if (irAstCfoldMode) {
                doIRAstCfold(arguments);
            } else if (irCfoldMode) {
                doIRCfold(arguments);
            } else if (lowerMode) {
                doIRLower(arguments);
            } else if (blkReorderMode) {
                doIRBlkReorder(arguments);
            } else if (irGenMode) {
                doIRGen(arguments);
            } else if (basicIRMode) {
                doBasicIR(arguments);
            } else if (noOptimize){
                doAsmGenNoOpt(arguments);
            } else {
                doAsmGen(arguments);
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
