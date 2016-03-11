package edu.cornell.cs.cs4120.xic.ir.interpret;

/**
 * Constants for interpreting intermediate code.
 */
public class Configuration {
    /* Some special stack-related names that are used in the IR */
    /** Prefix for argument registers */
    public static final String ABSTRACT_ARG_PREFIX = "_ARG";
    /** Prefix for return registers */
    public static final String ABSTRACT_RET_PREFIX = "_RET";

    /** Word size; assumes a 64-bit architecture */
    public static final int WORD_SIZE = 8;
}
