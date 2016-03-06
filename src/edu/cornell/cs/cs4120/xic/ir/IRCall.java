package edu.cornell.cs.cs4120.xic.ir;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import edu.cornell.cs.cs4120.util.SExpPrinter;
import edu.cornell.cs.cs4120.xic.ir.visit.IRVisitor;

/**
 * An intermediate representation for a function call
 * CALL(e_target, e_1, ..., e_n)
 */
public class IRCall extends IRExpr {
    private IRExpr target;
    private List<IRExpr> args;
    int returnSize;

    /**
     *
     * @args target address of the code for this function call
     * @args args arguments of this function call
     */
    public IRCall(IRExpr target, List<IRExpr> args, int returnSize) {
        this.target = target;
        this.args = args;
        this.returnSize = returnSize;
    }
    
    public IRCall(IRExpr target, int returnSize, IRExpr... args) {
    	this.target = target;
    	this.args = Arrays.asList(args);
    	this.returnSize = returnSize;    	
    }


    public IRExpr target() {
        return target;
    }

    public List<IRExpr> args() {
        return args;
    }

    public int returnSize() {
        return returnSize;
    }

    public boolean isToProcedure() {
        return returnSize == 0;
    }

    @Override
    public String label() {
        return "CALL";
    }

    @Override
    public IRNode visitChildren(IRVisitor v) {
        boolean modified = false;

        IRExpr target = (IRExpr) v.visit(this, this.target);
        if (target != this.target) modified = true;

        List<IRExpr> results = new ArrayList<>(args.size());
        for (IRExpr args : args) {
            IRExpr newExpr = (IRExpr) v.visit(this, args);
            if (newExpr != args) modified = true;
            results.add(newExpr);
        }

        if (modified) return new IRCall(target, results, returnSize);

        return this;
    }

    @Override
    public void printSExp(SExpPrinter p) {
        p.startList();
        p.printAtom("CALL");
        target.printSExp(p);
        for (IRExpr arg : args)
            arg.printSExp(p);
        p.endList();
    }

    @Override
    public boolean containsCalls() {
        return true;
    }

    @Override
    public int computeMaximumCallResults() {
        return Math.max(target.computeMaximumCallResults(), returnSize);
    }


}
