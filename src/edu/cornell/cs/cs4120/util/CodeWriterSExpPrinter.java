package edu.cornell.cs.cs4120.util;

import java.io.IOException;
import java.io.OutputStream;

import polyglot.util.CodeWriter;
import polyglot.util.OptimalCodeWriter;

/**
 * An {@linkplain SExpPrinter} implementation designed to print ASTs through a
 * provided {@link CodeWriter}.
 */
public class CodeWriterSExpPrinter implements SExpPrinter {
    private final CodeWriter writer;

    private boolean addSpace;

    /**
     * Constructs a new {@linkplain SExpPrinter} instance that prints programs
     * using the given {@code CodeWriter}.
     *
     * @param writer
     *          the {@code CodeWriter} to print to
     */
    public CodeWriterSExpPrinter(CodeWriter writer) {
        this.writer = writer;
    }

    /**
     * Constructs a new {@linkplain SExpPrinter} instance that prints programs
     * using to the given stream.  Output is kept to 80 columns.
     *
     * @param o
     *          the output stream to print to
     */
    public CodeWriterSExpPrinter(OutputStream o) {
        this(new OptimalCodeWriter(o, 80));
    }

    @Override
    public void printAtom(String atom) {
        if (addSpace)
            writer.allowBreak(0);
        else addSpace = true;
        writer.write(atom);
    }

    @Override
    public void startList() {
        if (addSpace)
            writer.allowBreak(0);
        else addSpace = true;
        addSpace = false;
        writer.begin(1);
        writer.write("(");
    }

    @Override
    public void endList() {
        writer.write(")");
        writer.end();
        addSpace = true;
    }

    @Override
    public void flush() {
        try {
            writer.flush();
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
