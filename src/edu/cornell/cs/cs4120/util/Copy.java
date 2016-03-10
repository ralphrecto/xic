package edu.cornell.cs.cs4120.util;

/**
 * Interface used to copy objects.  Similar to {@code Cloneable}, except that
 * {@code copy()} must be public, not protected as {@code clone()} is.
 */
public interface Copy<T> extends Cloneable {
    public T copy();

    class Util {
        @SuppressWarnings("unchecked")
        public static <T extends Copy<? super T>> T copy(T o) {
            return (T) o.copy();
        }
    }
}
