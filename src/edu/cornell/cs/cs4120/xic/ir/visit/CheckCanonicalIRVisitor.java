package edu.cornell.cs.cs4120.xic.ir.visit;

import edu.cornell.cs.cs4120.xic.ir.IRNode;

public class CheckCanonicalIRVisitor extends AggregateVisitor<Boolean> {

    protected boolean inSeq;
    protected boolean inExpr;

    @Override
    public Boolean unit() {
        return true;
    }

    @Override
    public Boolean bind(Boolean r1, Boolean r2) {
        return r1 && r2;
    }

    @Override
    protected CheckCanonicalIRVisitor enter(IRNode parent, IRNode n) {
        return n.checkCanonicalEnter(this);
    }

    @Override
    protected Boolean leave(IRNode parent, IRNode n, Boolean r,
            AggregateVisitor<Boolean> v_) {
        return r && n.isCanonical(this);
    }

    /**
     * Record that a SEQ is being entered.
     * @return the updated visitor
     */
    public CheckCanonicalIRVisitor enterSeq() {
        CheckCanonicalIRVisitor v = this;
        if (!v.inSeq) {
            v = copyIfNeeded(v);
            v.inSeq = true;
        }
        return v;
    }

    /**
     * Record that an IR expression is being entered.
     * @return the updated visitor
     */
    public CheckCanonicalIRVisitor enterExpr() {
        CheckCanonicalIRVisitor v = this;
        if (!v.inExpr) {
            v = copyIfNeeded(v);
            v.inExpr = true;
        }
        return v;
    }

    /**
     *
     * @return true if the visitor is inside a SEQ; false otherwise
     */
    public boolean inSeq() {
        return inSeq;
    }

    /**
     *
     * @return true if the visitor is inside an expression; false otherwise
     */
    public boolean inExpr() {
        return inExpr;
    }
}
