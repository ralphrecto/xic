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
        INTEGER_LITERAL_OUT_OF_BOUNDS,
		INVALID_HEX_ESCAPE,
		INVALID_UNICODE_ESCAPE,
		INVALID_ESCAPE,
		INVALID_CHAR_CONSTANT,
		UNCLOSED_STRING_LITERAL,
		UNCLOSED_CHAR_LITERAL
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

	public static class InvalidHexEscapeException extends XicException {
		public InvalidHexEscapeException(int row, int column, String s) {
			super(ErrorCode.INVALID_HEX_ESCAPE, row, column, String.format(
				  "error:Invalid hex escape %s", s));
		}
	}

	public static class InvalidUnicodeEscapeException extends XicException {
		public InvalidUnicodeEscapeException(int row, int column, String s) {
			super(ErrorCode.INVALID_UNICODE_ESCAPE, row, column, String.format(
				  "error.Invalid unicode escape %s", s));
		}
	}

	public static class InvalidEscapeException extends XicException {
		public InvalidEscapeException(int row, int column, String s) {
			super(ErrorCode.INVALID_ESCAPE, row, column, String.format(
				  "error:Invalid escape %s", s));
		} 
	}
	
	public static class InvalidCharacterConstantException extends XicException {
		public InvalidCharacterConstantException(int row, int column) {
			super(ErrorCode.INVALID_CHAR_CONSTANT, row, column,
            	  "error:Invalid character constant");
		}
	}

	public static class UnclosedStringLiteralException extends XicException {
		public UnclosedStringLiteralException(int row, int column, String s) {
			super(ErrorCode.UNCLOSED_STRING_LITERAL, row, column, String.format(
				  "error:Unclosed string literal %s", s));
		}
	}

	public static class UnclosedCharacterLiteralException extends XicException {
		public UnclosedCharacterLiteralException(int row, int column, String s) {
			super(ErrorCode.UNCLOSED_CHAR_LITERAL, row, column, String.format(
				  "error:Unclosed char literal %s", s));
		}
	}
}









