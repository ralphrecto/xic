package edu.cornell.cs.cs4120.xic.ir;

import java.io.PrintWriter;
import java.io.StringWriter;

import edu.cornell.cs.cs4120.util.CodeWriterSExpPrinter;
import edu.cornell.cs.cs4120.util.SExpPrinter;
import edu.cornell.cs.cs4120.xic.ir.visit.AggregateVisitor;
import edu.cornell.cs.cs4120.xic.ir.visit.CheckCanonicalIRVisitor;
import edu.cornell.cs.cs4120.xic.ir.visit.IRVisitor;
import edu.cornell.cs.cs4120.xic.ir.visit.InsnMapsBuilder;

/**
 * A node in an intermediate-representation abstract syntax tree.
 */
public abstract class IRNode {

    /**
     * Visit the children of this IR node.
     * @param v the visitor
     * @return the result of visiting children of this node
     */
    public IRNode visitChildren(IRVisitor v) {
        return this;
    }

    public <T> T aggregateChildren(AggregateVisitor<T> v) {
        return v.unit();
    }

    public InsnMapsBuilder buildInsnMapsEnter(InsnMapsBuilder v) {
        return v;
    }

    public IRNode buildInsnMaps(InsnMapsBuilder v) {
        v.addInsn(this);
        return this;
    }

    public CheckCanonicalIRVisitor checkCanonicalEnter(
            CheckCanonicalIRVisitor v) {
        return v;
    }

    public boolean isCanonical(CheckCanonicalIRVisitor v) {
        return true;
    }

    public abstract String label();

    /**
     * Print an S-expression representation of this IR node.
     * @param p the S-expression printer
     */
    public abstract void printSExp(SExpPrinter p);

    @Override
    public String toString() {
        StringWriter sw = new StringWriter();
        try (PrintWriter pw = new PrintWriter(sw);
             SExpPrinter sp = new CodeWriterSExpPrinter(pw)) {
            printSExp(sp);
        }
        return sw.toString();
    }
}
