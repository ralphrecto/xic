package edu.cornell.cs.cs4120.xic.ir;

import java.util.ArrayList;
import java.util.List;

import edu.cornell.cs.cs4120.util.SExpPrinter;
import edu.cornell.cs.cs4120.xic.ir.visit.IRVisitor;

/**
 * An intermediate representation for a sequence of statements
 * SEQ(s1,...,sn)
 */
public class IRSeq extends IRStmt {
    private List<IRStmt> stmts;

    /**
     * Create a SEQ from a list of statements. The list should not be modified subsequently.
     * @param stmts the sequence of statements
     */
    private IRSeq(List<IRStmt> stmts) {
        this.stmts = stmts;
    }

    /**
     * @param st the statements
     */
    public IRSeq(IRStmt... st) {
        stmts = new ArrayList<>(st.length);
        for (IRStmt s : st)
            stmts.add(s);
    }

    public List<IRStmt> stmts() {
        return stmts;
    }

    @Override
    public String label() {
        return "SEQ";
    }

    @Override
    public IRNode visitChildren(IRVisitor v) {
        boolean modified = false;

        List<IRStmt> results = new ArrayList<>(stmts.size());
        for (IRStmt stmt : stmts) {
            IRStmt newStmt = (IRStmt) v.visit(this, stmt);
            if (newStmt != stmt) modified = true;
            results.add(newStmt);
        }

        if (modified) return new IRSeq(results);

        return this;
    }

    @Override
    public void printSExp(SExpPrinter p) {
        p.startList();
        p.printAtom("SEQ");
        for (IRStmt stmt : stmts)
            stmt.printSExp(p);
        p.endList();
    }

    @Override
    public boolean containsCalls() {
        for (IRStmt stmt : stmts)
            if (stmt.containsCalls()) return true;
        return false;
    }

    @Override
    public int computeMaximumCallResults() {
        int value = 0;
        for (IRStmt s : stmts)
            value = Math.max(value, s.computeMaximumCallResults());
        return value;
    }

}
