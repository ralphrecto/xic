package edu.cornell.cs.cs4120.xic.ir.visit;

import edu.cornell.cs.cs4120.xic.InternalCompilerError;
import edu.cornell.cs.cs4120.xic.ir.IRNode;

public abstract class IRVisitor {
    /**
     * Recursively traverse the IR subtree rooted at {@code n}
     */
    public IRNode visit(IRNode parent, IRNode n) {
        if (n == null) return null;

        /* Allow the visitor implementation to hijack traversal of n */
        IRNode overrideValue = override(parent, n);
        if (overrideValue != null) return overrideValue;

        IRVisitor v_ = enter(parent, n);
        if (v_ == null)
            throw new InternalCompilerError("IRVisitor.enter() returned null!");

        IRNode n_ = n.visitChildren(v_);
        if (n_ == null)
            throw new InternalCompilerError("IRVisitor.visitChildren() returned null!");

        n_ = leave(parent, n, n_, v_);
        if (n_ == null)
            throw new InternalCompilerError("IRVisitor.leave() returned null!");

        return n_;
    }

    /**
     * Recursively traverse the IR subtree rooted at {@code n}
     */
    public IRNode visit(IRNode node) {
        return visit(null, node);
    }

    /**
     * Allows to hijack the traversal of a subtree. This function is called by
     * {@link #visit(IRNode, IRNode)} upon entering node {@code n}.
     * If a non-null node {@code n0} is returned, the traversal is stopped
     * and the resulting AST has {@code n0} in place of {@code n}.
     *
     * By default, overriding is inactive.
     */
    protected IRNode override(IRNode parent, IRNode n) {
        return null;
    }

    /**
     * Called upon entering {@code n} during the AST traversal. This allows
     * to perform certain actions, including returning a new Node visitor to be
     * used in the subtree.
     */
    protected IRVisitor enter(IRNode parent, IRNode n) {
        return this;
    }

    /**
     * Called after finishing traversal of the subtree rooted at {@code n}.
     * When {@link #enter(IRNode, IRNode)} creates a new visitor to be used on
     * the subtree, the old visitor still receives the call to {@code leave()}
     * -- that is, {@code leave()} always executed the same number of times
     * as {@link #enter(IRNode, IRNode)}.
     * This node provides the final opportunity of placing an updated node
     * in the output AST.
     *
     * @param parent
     *            The parent AST node of {@code n} or {@code null}
     *            when it is the root.
     * @param n
     *            The original node in the input AST
     * @param n_
     *            The node returned by {@link IRNode#visitChildren(IRVisitor)}
     * @param v_
     *            The new node visitor created by
     *            {@link #enter(IRNode, IRNode)}, or {@code this}.
     */

    protected IRNode leave(IRNode parent, IRNode n, IRNode n_,
            IRVisitor v_) {
        return n_;
    }
}
