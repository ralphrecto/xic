package edu.cornell.cs.cs4120.xic.ir;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;

import edu.cornell.cs.cs4120.util.SExpPrinter;
import edu.cornell.cs.cs4120.xic.ir.visit.IRVisitor;

/**
 * An intermediate representation for a compilation unit
 */
public class IRCompUnit extends IRNode {
    private String name;
    private Map<String, IRFuncDecl> functions;
    private IRSeq seq;

    public IRCompUnit(String name) {
        this.name = name;
        functions = new LinkedHashMap<>();
    }

    public IRCompUnit(String name, Map<String, IRFuncDecl> functions) {
        this(name, functions, null);
    }

    public IRCompUnit(String name, Map<String, IRFuncDecl> functions,
            IRSeq seq) {
        this.name = name;
        this.functions = functions;
        this.seq = seq;
    }

    public void appendFunc(IRFuncDecl func) {
        functions.put(func.name(), func);
    }

    public String name() {
        return name;
    }

    public Map<String, IRFuncDecl> functions() {
        return functions;
    }

    public IRFuncDecl getFunction(String name) {
        return functions.get(name);
    }

    @Override
    public String label() {
        return "COMPUNIT";
    }

    @Override
    public IRNode visitChildren(IRVisitor v) {
        boolean modified = false;

        Map<String, IRFuncDecl> results = new TreeMap<>();
        for (String funcName : functions.keySet()) {
            IRFuncDecl func = functions.get(funcName);
            IRFuncDecl newFunc = (IRFuncDecl) v.visit(this, func);
            if (newFunc != func) modified = true;
            results.put(newFunc.name(), newFunc);
        }

        IRSeq newSeq = seq;

        if (hasExtraSequence()) {
            newSeq = (IRSeq) v.visit(this, seq);
            if (newSeq != seq) modified = true;
        }

        if (modified) return new IRCompUnit(name, results, newSeq);

        return this;
    }

    @Override
    public void printSExp(SExpPrinter p) {
        p.startList();
        p.printAtom("COMPUNIT");
        for (IRFuncDecl func : functions.values())
            func.printSExp(p);
        p.endList();
    }

    public boolean hasExtraSequence() {
        return seq != null;
    }

    @Override
    public boolean containsCalls() {
        for (IRFuncDecl func : functions.values())
            if (func.containsCalls()) return true;
        if (hasExtraSequence()) return true;
        return false;
    }

    @Override
    public int computeMaximumCallResults() {
        int value = 0;
        for (IRFuncDecl f : functions.values())
            value = Math.max(value, f.computeMaximumCallResults());
        return value;
    }

}
