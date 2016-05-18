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
    /** Prefix for global variables */
    public static final String GLOBAL_PREFIX = "_I_g_";
    /** Prefix for dispatch vectors */
    public static final String DISPATCH_VECTOR_PREFIX = "_I_vt_";
    /** Prefix for class sizes */
    public static final String SIZE_PREFIX = "_I_size_";

    /** Word size; assumes a 64-bit architecture */
    public static final int WORD_SIZE = 8;
}
