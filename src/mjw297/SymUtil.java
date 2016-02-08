package mjw297;

import java_cup.runtime.Symbol;

import java.util.function.Function;

/**
 * Lexing helper class. A catch-all for any helper functions used for lexing.
 */
public class SymUtil {
    /**
     * Get string literals for Symbols.
     * <p>
     * For String and Char literals, chars in
     * the range [0x20,0x7E] are printed; all other characters are escaped.
     * Commonly escaped characters (e.g. newline, tab, etc.) are pretty printed
     * when escaped to helpful values (e.g. newline as '\n'), and all other
     * characters outside of the printable range will be escaped with its
     * code point.
     * </p>
     *
     * @param sym A Symbol object
     * @return String escaped/pretty printed string for the Symbol
     */
    public static String symToLiteral(Symbol sym) {
        Function<String, String> prettyPrint = (s) -> {
            StringBuilder out = new StringBuilder();
            for (int i = 0; i < s.length(); i++) {
                int codePoint = s.codePointAt(i);
                if (0x20 <= codePoint && codePoint <= 0x7E) {
                    out.append(s.charAt(i));
                } else {
                    switch (s.charAt(i)) {
                        case ('\t'): out.append("\\t"); break;
                        case ('\b'): out.append("\\b"); break;
                        case ('\n'): out.append("\\n"); break;
                        case ('\r'): out.append("\\r"); break;
                        case ('\f'): out.append("\\f"); break;
                        case ('\''): out.append("\\'"); break;
                        case ('\"'): out.append("\\\""); break;
                        case ('\\'): out.append("\\\\"); break;
                        default: out.append("\\x" + Integer.toHexString(codePoint));
                    }
                }
            }
            return out.toString();
        };

        String terminalName = Sym.terminalNames[sym.sym];
        switch (terminalName) {
            case ("EOF"): return "EOF";
            case ("error"): return "error";
            case ("MINUS"): return "-";
            case ("BANG"): return "!";
            case ("STAR"): return "*";
            case ("HIGHMULT"): return "*>>";
            case ("DIV"): return "/";
            case ("MOD"): return "%";
            case ("PLUS"): return "+";
            case ("EQ"): return "=";
            case ("LT"): return "<";
            case ("LTE"): return "<=";
            case ("GTE"): return ">=";
            case ("GT"): return ">";
            case ("EQEQ"): return "==";
            case ("NEQ"): return "!=";
            case ("AMP"): return "&";
            case ("BAR"): return "|";
            case ("SEMICOLON"): return ";";
            case ("LPAREN"): return "(";
            case ("RPAREN"): return ")";
            case ("LBRACKET"): return "[";
            case ("RBRACKET"): return "]";
            case ("LBRACE"): return "{";
            case ("RBRACE"): return "}";
            case ("UNDERSCORE"): return "_";
            case ("COMMA"): return ",";
            case ("COLON"): return ":";
            case ("STRING"): return "string " +
                    prettyPrint.apply((String) sym.value);
            case ("CHAR"): return "character " +
                    prettyPrint.apply(sym.value.toString());
            case ("ID"): return "id " + sym.value;
            case ("NUM"): return "integer " + sym.value;
            case ("BIG_NUM"): return "integer " + sym.value;
            case ("WHILE"):
            case ("INT"):
            case ("BOOL"):
            case ("IF"):
            case ("ELSE"):
            case ("RETURN"):
            case ("USE"):
            case ("LENGTH"):
            case ("TRUE"):
            case ("FALSE"): return terminalName.toLowerCase();
            default: return terminalName.toLowerCase();
        }
    }
}
