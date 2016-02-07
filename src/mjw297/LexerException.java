package mjw297;

/**
 * A {@code LexerException} is an exception that occurs during lexing. For
 * example, unterminated string literals, invalid integer literals, and invalid
 * escape characters would all throw LexerExceptions. Each {@code
 * LexerException} is accompanied by the row and column where the lexing
 * exception originated as well as a detailed error message explaining the
 * error. These exceptions can be caught by the main xic program and pretty
 * printed.
 */
@SuppressWarnings("serial")
public class LexerException extends Exception {
    public final ErrorCode code;
    public final int row;
    public final int column;

    /**
     * {@code LexerException(r, c, m)} constructs a LexerException that
     * occurred at row {@code r} and column {@code c} with detailed error
     * message {@code message}.
     */
    public LexerException(ErrorCode code, int row, int column, String message) {
        super(message);
        this.code = code;
        this.row = row;
        this.column = column;
    }
}
