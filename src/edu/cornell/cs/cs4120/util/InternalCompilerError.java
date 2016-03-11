package edu.cornell.cs.cs4120.util;

import polyglot.util.SerialVersionUID;

/** Exception thrown when the compiler is confused. */
public class InternalCompilerError extends RuntimeException {
    private static final long serialVersionUID = SerialVersionUID.generate();

    public InternalCompilerError(String msg) {
        super(msg);
    }

    public InternalCompilerError(Throwable cause) {
        this(cause.getMessage(), cause);
    }

    public InternalCompilerError(String msg, Throwable cause) {
        super(msg, cause);
    }
}
