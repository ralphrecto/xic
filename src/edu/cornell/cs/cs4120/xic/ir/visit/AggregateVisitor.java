package edu.cornell.cs.cs4120.xic.ir.visit;

import edu.cornell.cs.cs4120.util.Copy;
import edu.cornell.cs.cs4120.util.InternalCompilerError;
import edu.cornell.cs.cs4120.xic.ir.IRNode;

public abstract class AggregateVisitor<T> implements Copy<AggregateVisitor<T>> {

    /**
     *
     * @return the unit of the aggregate
     */
    public abstract T unit();

    /**
     * Bind two aggregate results
     * @param r1
     * @param r2
     * @return
     */
    public abstract T bind(T r1, T r2);

    /**
     * Recursively traverse the IR subtree rooted at {@code n}
     */
    public T visit(IRNode parent, IRNode n) {
        if (n == null) return null;

        /* Allow the visitor implementation to hijack traversal of n */
        T overrideValue = override(parent, n);
        if (overrideValue != null) return overrideValue;

        AggregateVisitor<T> v_ = enter(parent, n);
        if (v_ == null)
            throw new InternalCompilerError("AggregateVisitor.enter() returned null!");

        T r = n.aggregateChildren(v_);
        if (r == null)
            throw new InternalCompilerError("AggregateVisitor.visitChildren() returned null!");

        r = leave(parent, n, r, v_);
        if (r == null)
            throw new InternalCompilerError("AggregateVisitor.leave() returned null!");

        return r;
    }

    /**
     * Recursively traverse the IR subtree rooted at {@code n}
     */
    public T visit(IRNode node) {
        return visit(null, node);
    }

    /**
     * Allows to hijack the traversal of a subtree. This function is called by
     * {@link #visit(IRNode, IRNode)} upon entering node {@code n}.
     * If a non-null result {@code r} is returned, the traversal is stopped
     * and the result is {@code r}.
     *
     * By default, overriding is inactive.
     */
    protected T override(IRNode parent, IRNode n) {
        return null;
    }

    /**
     * Called upon entering {@code n} during the AST traversal. This allows
     * to perform certain actions, including returning a new visitor to be
     * used in the subtree.
     */
    protected AggregateVisitor<T> enter(IRNode parent, IRNode n) {
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
     * @param r
     *            The result returned by {@link IRNode#aggregateChildren(AggregateVisitor)}
     * @param v_
     *            The new node visitor created by
     *            {@link #enter(IRNode, IRNode)}, or {@code this}.
     */

    protected T leave(IRNode parent, IRNode n, T r, AggregateVisitor<T> v_) {
        return r;
    }

    /**
     * Return a clone of this visitor if the given visitor is this visitor,
     * or the given visitor otherwise.
     * @param v the visitor
     * @return a clone of v if v == this, or v otherwise
     */
    protected <V extends AggregateVisitor<T>> V copyIfNeeded(V v) {
        if (v == this) return Copy.Util.copy(v);
        return v;
    }

    @Override
    @SuppressWarnings("unchecked")
    public AggregateVisitor<T> copy() {
        try {
            return (AggregateVisitor<T>) super.clone();
        }
        catch (CloneNotSupportedException e) {
            throw new InternalCompilerError("Java clone() weirdness.");
        }
    }
}
