package mjw297;

/**
 * A {@code XicException} is an exception that occurs in the Xi compiler. For
 * example, unterminated string literals, invalid integer literals, and invalid
 * escape characters would all throw XicExceptions. Each {@code XicExceptions}
 * is accompanied by a unique error code, the row and column where the
 * exception originated, as well as a detailed error message explaining the
 * error. These exceptions can be caught by the main xic program and pretty
 * printed.
 */
@SuppressWarnings("serial")
public abstract class XicException extends Exception {
    public enum ErrorCode {
        INTEGER_LITERAL_OUT_OF_BOUNDS
    }

    public final ErrorCode code;
    public final int row;
    public final int column;

    /**
     * {@code XicException(r, c, m)} constructs a XicException that occurred at
     * row {@code r} and column {@code c} with detailed error message {@code
     * message}.
     */
    public XicException(ErrorCode code, int row, int column, String message) {
        super(message);
        this.code = code;
        this.row = row;
        this.column = column;
    }

    public static class IntegerLiteralOutOfBoundsException extends XicException {
        public IntegerLiteralOutOfBoundsException(int row, int column, String s) {
            super(ErrorCode.INTEGER_LITERAL_OUT_OF_BOUNDS, row, column, String.format(
                  "error:Integer literal %s out of bounds [0, 9223372036854775807]", s));
        }
    }
}
