package edu.cornell.cs.cs4120.xic.ir.visit;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import edu.cornell.cs.cs4120.util.InternalCompilerError;
import edu.cornell.cs.cs4120.xic.ir.IRNode;

public class InsnMapsBuilder extends IRVisitor {
    private Map<String, Long> nameToIndex;
    private Map<Long, IRNode> indexToInsn;
    private Map<IRNode, Long> insnToIndex;
    private List<String> ctors;

    private long index;

    public InsnMapsBuilder() {
        nameToIndex = new HashMap<>();
        indexToInsn = new HashMap<>();
        insnToIndex = new HashMap<>();
        ctors = new LinkedList<>();
        index = 0;
    }

    public Map<String, Long> nameToIndex() {
        return nameToIndex;
    }

    public Map<Long, IRNode> indexToInsn() {
        return indexToInsn;
    }

    public Map<IRNode, Long> insnToIndex() {
        return insnToIndex;
    }

    public List<String> ctors() {
        return ctors;
    }

    @Override
    protected IRVisitor enter(IRNode parent, IRNode n) {
        InsnMapsBuilder v = n.buildInsnMapsEnter(this);
        return v;
    }

    @Override
    protected IRNode leave(IRNode parent, IRNode n, IRNode n_, IRVisitor v_) {
        return n_.buildInsnMaps((InsnMapsBuilder) v_);
    }

    public void addInsn(IRNode n) {
        indexToInsn.put(index, n);
        if (insnToIndex.containsKey(n))
            throw new InternalCompilerError("Error - encountered "
                    + "duplicate node " + n.label()
                    + " in the IR tree -- go fix the generator.");
        insnToIndex.put(n, index);
        index++;
    }

    public void addNameToCurrentIndex(String name) {
        if (nameToIndex.containsKey(name))
            throw new InternalCompilerError("Error - encountered "
                    + "duplicate name " + name
                    + " in the IR tree -- go fix the generator.");
        nameToIndex.put(name, index);
    }
}
