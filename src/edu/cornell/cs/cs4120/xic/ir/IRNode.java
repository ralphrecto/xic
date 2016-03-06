package edu.cornell.cs.cs4120.xic.ir;

import edu.cornell.cs.cs4120.util.SExpPrinter;
import edu.cornell.cs.cs4120.xic.ir.visit.IRVisitor;
import edu.cornell.cs.cs4120.xic.ir.visit.InsnMapsBuilder;

/**
 * A node in an intermediate-representation abstract syntax tree.
 */
public abstract class IRNode {

    public IRNode visitChildren(IRVisitor v) {
        return this;
    }

    public InsnMapsBuilder buildInsnMapsEnter(InsnMapsBuilder v) {
        return v;
    }

    public IRNode buildInsnMaps(InsnMapsBuilder v) {
        v.addInsn(this);
        return this;
    }

    public abstract String label();

    public abstract void printSExp(SExpPrinter p);

    public abstract boolean containsCalls();

    /** Max number of results of any call in the subtree. */
    public abstract int computeMaximumCallResults();
}
