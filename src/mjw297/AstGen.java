package mjw297;

import com.google.common.collect.Lists;
import javafx.geometry.Pos;

import java.util.*;

import static mjw297.Ast.*;

public class AstGen {

    private static final Random rand = new Random();
    private static final Position dummyPos = new Position(-1, -1);

    static <T> List<T> repeat(java.util.concurrent.Callable<T> f, int times) {
        List<T> ret = new ArrayList<>();
        for (int i = 0; i < times; i++) {
            try {
                ret.add(f.call());
            } catch (Exception e) {
                return ret;
            }
        }
        return ret;
    }

    static <T> T choose(List<T> choices) {
        return choices.get(rand.nextInt(choices.size()));
    }

    static StringBuilder sbFlatten(List<StringBuilder> l, String delimiter) {
        StringBuilder ret = new StringBuilder();
        if (l.size() > 0) {
            for (int i = 0; i < l.size() - 1; i++) {
                ret.append(l.get(i));
                ret.append(delimiter);
            }
            ret.append(l.get(l.size() - 1));
        }
        return ret;
    }

    private static final int genInt(Integer limit) {
        return 1 + rand.nextInt(limit);
    }

    static String genString() {
        StringBuilder sb = new StringBuilder();
        int len = rand.nextInt(15);
        for (int i = 0; i < len + 1; i++) {
            char c = (char) (rand.nextInt(26) +
                    (rand.nextBoolean() ? 'a' : 'A'));
            sb.append(c);
        }
        return sb.toString() + genInt(10);
    }

    static char genChar() {
        return (char) (rand.nextInt(26) + (rand.nextBoolean() ? 'a' : 'A'));
    }

    static Program<Position> genProgram() {
        return Program.of(
                dummyPos,
                repeat(AstGen::genUse, genInt(15)),
                repeat(AstGen::genFunc, genInt(10))
        );
    }

    static Use<Position> genUse() {
        return Use.of(dummyPos, genId());
    }

    static Callable<Position> genFunc() {
        return Func.of(
                dummyPos,
                genId(),
                repeat(AstGen::genAVar, genInt(10)),
                repeat(AstGen::genType, genInt(5)),
                genBlock()
        );
    }

    static Callable<Position> genProc() {
        return Proc.of(
                dummyPos,
                genId(),
                repeat(AstGen::genAVar, genInt(10)),
                genBlock()
        );
    }

    static AnnotatedVar<Position> genAVar() {
        return AnnotatedId.of(
                dummyPos,
                genId(),
                genType()
        );
    }

    static Underscore<Position> genUnderscore() {
        return Underscore.of(dummyPos);
    }

    static Var<Position> genDeclVar() {
        switch (genInt(2)) {
            case 1: return genAVar();
            default: return genUnderscore();
        }
    }

    static Int<Position> genIntType() {
        return Int.of(dummyPos);
    }

    static Bool<Position> genBoolType() {
        return Bool.of(dummyPos);
    }

    static Array<Position> genArrayType() {
        return Array.of(dummyPos, genType(), Optional.empty());
    }

    static Type<Position> genType() {
        switch (genInt(3)) {
            case 1: return genIntType();
            case 2: return genBoolType();
            default: return genArrayType();
        }
    }

    /* Statements */

    static Block<Position> genBlock() {
        return Block.of(
            dummyPos,
            repeat(AstGen::genStmt, 10),
            Optional.of(repeat(AstGen::genExpr, 10))
        );
    }

    static Decl<Position> genDecl() {
        return Decl.of(
                dummyPos,
                repeat(AstGen::genDeclVar, genInt(6))
        );
    }

    static DeclAsgn<Position> genDeclAsgn() {
        return DeclAsgn.of(
                dummyPos,
                repeat(AstGen::genDeclVar, genInt(6)),
                genExpr()
        );
    }

    static Asgn<Position> genAsgn() {
        return Asgn.of(
                dummyPos,
                genExpr(),
                genExpr()
        );
    }

    static If<Position> genIf() {
        return If.of(
                dummyPos,
                genExpr(),
                genBlock()
        );
    }

    static IfElse<Position> genIfElse() {
        return IfElse.of(
                dummyPos,
                genExpr(),
                genBlock(),
                genBlock()
        );
    }

    static While<Position> genWhile() {
        return While.of(
                dummyPos,
                genExpr(),
                genBlock()
        );
    }

    static Stmt<Position> genBaseStmt() {
        switch (genInt(3)) {
            case 1: return genDecl();
            case 2: return genDeclAsgn();
            default: return genAsgn();
        }
    }

    static Stmt<Position> genStmt() {
        if (rand.nextBoolean()) {
            return genBaseStmt();
        }
        switch (genInt(3)) {
            case 1: return genIf();
            case 2: return genIfElse();
            default: return genWhile();
        }
    }

    /* Expressions */

    static Id<Position> genId() {
        return Id.of(dummyPos, genString());
    }

    static BinOp<Position> genBinOp() {
        BinOpCode[] binops = BinOpCode.values();
        return BinOp.of(
                dummyPos,
                binops[rand.nextInt(binops.length)],
                genExpr(),
                genExpr()
        );
    }

    static UnOp<Position> genUnOp() {
        UnOpCode[] unops = UnOpCode.values();
        return UnOp.of(
                dummyPos,
                unops[rand.nextInt(unops.length)],
                genExpr()
        );
    }

    static Length<Position> genLength() {
        return Length.of(dummyPos, genExpr());
    }

    static NumLiteral<Position> genNumLit() {
        return NumLiteral.of(dummyPos, (long) genInt(Integer.MAX_VALUE));
    }

    static CharLiteral<Position> genCharLit() {
        return CharLiteral.of(dummyPos, genChar());
    }

    static ArrayLiteral<Position> genArrayLit() {
        return ArrayLiteral.of(dummyPos,
                repeat(AstGen::genBaseExpr, genInt(5))
        );
    }

    static StringLiteral<Position> genStringLit() {
        return StringLiteral.of(dummyPos, genString());
    }

    static Expr<Position> genBaseExpr() {
        switch (genInt(4)) {
            case 1: return genId();
            case 2: return genNumLit();
            case 3: return genStringLit();
            default: return genCharLit();
        }
    }

    static Expr<Position> genExpr() {
        /* generate a base expr or a complex expr */
        if (rand.nextBoolean()) {
            return genBaseExpr();
        }

        switch (genInt(4)) {
            case 1: return genBinOp();
            case 2: return genUnOp();
            case 3: return genLength();
            default: return genArrayLit();
        }
    }

}