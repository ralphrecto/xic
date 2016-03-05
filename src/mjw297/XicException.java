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
	public enum ErrorType {
		LEXING,
		SYNTACTIC,
		IO
	}

    public enum ErrorCode {
        INTEGER_LITERAL_OUT_OF_BOUNDS,
		INVALID_HEX_ESCAPE,
		INVALID_UNICODE_ESCAPE,
		INVALID_ESCAPE,
		EMPTY_CHAR_LITERAL,
		INVALID_CHAR_CONSTANT,
		INVALID_TOKEN,
		UNCLOSED_STRING_LITERAL,
		UNCLOSED_CHAR_LITERAL,
        SYNTAX,
		IOERROR,
		USEERROR
    }

    public final ErrorCode code;
    public final int row;
    public final int column;
	public final ErrorType type;

    /**
     * {@code XicException(r, c, m)} constructs a XicException that occurred at
     * row {@code r} and column {@code c} with detailed error message {@code
     * message}.
     */
    public XicException(ErrorCode code, int row, int column, String message, ErrorType t) {
        super(message);
        this.code = code;
        this.row = row;
        this.column = column;
		this.type = t;
    }

    public static class IntegerLiteralOutOfBoundsException extends XicException {
        public IntegerLiteralOutOfBoundsException(int row, int column, String s) {
            super(ErrorCode.INTEGER_LITERAL_OUT_OF_BOUNDS, row, column, String.format(
                  "error:Integer literal %s out of bounds [0, 9223372036854775807]", s),
				ErrorType.LEXING);
        }
    }

	public static class InvalidHexEscapeException extends XicException {
		public InvalidHexEscapeException(int row, int column, String s) {
			super(ErrorCode.INVALID_HEX_ESCAPE, row, column, String.format(
				  "error:Invalid hex escape %s", s), ErrorType.LEXING);
		}
	}

	public static class InvalidUnicodeEscapeException extends XicException {
		public InvalidUnicodeEscapeException(int row, int column, String s) {
			super(ErrorCode.INVALID_UNICODE_ESCAPE, row, column, String.format(
				  "error.Invalid unicode escape %s", s), ErrorType.LEXING);
		}
	}

	public static class InvalidEscapeException extends XicException {
		public InvalidEscapeException(int row, int column, String s) {
			super(ErrorCode.INVALID_ESCAPE, row, column, String.format(
				  "error:Invalid escape %s", s), ErrorType.LEXING);
		}
	}

	public static class EmptyCharacterLiteralException extends XicException {
		public EmptyCharacterLiteralException(int row, int column) {
			super(ErrorCode.EMPTY_CHAR_LITERAL, row, column,
				  "error:empty character literal", ErrorType.LEXING);
		}
	}

	public static class InvalidCharacterConstantException extends XicException {
		public InvalidCharacterConstantException(int row, int column) {
			super(ErrorCode.INVALID_CHAR_CONSTANT, row, column,
            	  "error:Invalid character constant", ErrorType.LEXING);
		}
	}

	public static class InvalidTokenException extends XicException {
		public InvalidTokenException(int row, int column, String s) {
			super(ErrorCode.INVALID_TOKEN, row, column, String.format(
					"error:Cannot recognize token %s", s), ErrorType.LEXING);
		}
	}

	public static class UnclosedStringLiteralException extends XicException {
		public UnclosedStringLiteralException(int row, int column, String s) {
			super(ErrorCode.UNCLOSED_STRING_LITERAL, row, column, String.format(
				  "error:Unclosed string literal %s", s), ErrorType.LEXING);
		}
	}

	public static class UnclosedCharacterLiteralException extends XicException {
		public UnclosedCharacterLiteralException(int row, int column, String s) {
			super(ErrorCode.UNCLOSED_CHAR_LITERAL, row, column, String.format(
				  "error:Unclosed char literal %s", s), ErrorType.LEXING);
		}
	}

    public static class SyntaxException extends XicException {
        public SyntaxException(int row, int column, String symbol) {
            super(ErrorCode.SYNTAX, row, column, String.format(
                  "error:Unexpected token %s", row, column, symbol), ErrorType.SYNTACTIC);
        }
    }

	public static class XiIOException extends XicException {
		public XiIOException(String msg) {
			super(ErrorCode.IOERROR, -1, -1, msg, ErrorType.IO);
		}
	}

	public static class XiUseException extends XicException {
		public String useName;
		public XiUseException(String useName, int row, int col, String msg) {
			super(ErrorCode.USEERROR, row, col, String.format(
				"error: Use error: %s", row, col, msg
            ), ErrorType.SYNTACTIC);
			this.useName = useName;
		}
	}

	public static class GenericParserException extends XicException {
		public GenericParserException(int row, int col, String msg) {
			super(ErrorCode.SYNTAX, row, col, String.format(
                "error: parser error: %s", row, col, msg
			), ErrorType.SYNTACTIC);
		}
	}
}
