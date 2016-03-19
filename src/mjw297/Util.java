package mjw297;

import java.util.*;
import java.util.function.Function;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.ToString;

/**
 * Created by ralphrecto on 2/14/16.
 */
public class Util {
    @AllArgsConstructor(staticName="of")
    @EqualsAndHashCode
    @ToString(includeFieldNames=false)
    public static class Tuple<S,T> {
        public final S fst;
        public final T snd;
    }

    public static class Either<X, Y> {

        private Optional<X> left;
        private Optional<Y> right;

        public Either(Optional<X> left_, Optional<Y> right_) {
            left = left_;
            right = right_;
        }

        public <S> Either<S,Y> leftmap(Function<X,S> f) {
            if (isLeft()) {
                return Either.left(f.apply(left.get()));
            } else {
                return Either.right(right.get());
            }
        }

        public boolean isLeft() {
            return left.isPresent();
        }

        public boolean isRight() {
            return right.isPresent();
        }

        public X getLeft() {
            return left.get();
        }

        public Y getRight() {
            return right.get();
        }

        public static <X,Y> Either<X,Y> left(X left) {
            return new Either<>(Optional.of(left), Optional.empty());
        }

        public static <X,Y> Either<X,Y> right(Y right) {
            return new Either<>(Optional.empty(), Optional.of(right));
        }
    }

    static <X,Y> List<Tuple<X,Y>> zip(List<X> fsts, List<Y> snds) {
        List<Tuple<X,Y>> ret = new ArrayList<>();
        for (int i = 0; i < Math.min(fsts.size(), snds.size()); i++) {
            ret.add(new Tuple<>(fsts.get(i), snds.get(i)));
        }
        return ret;
    }

    static <X, Y> Tuple<List<X>, List<Y>> unzip(List<Tuple<X, Y>> xys) {
        List<X> xs = new ArrayList<>();
        List<Y> ys = new ArrayList<>();
        for (Tuple<X, Y> xy : xys) {
            xs.add(xy.fst);
            ys.add(xy.snd);
        }
        return new Tuple<>(xs, ys);
    }

    static <A> A choose(Set<A> xs) {
        int index = new Random().nextInt(xs.size());
        int i = 0;
        for(A x : xs) {
            if (i == index) {
                return x;
            }
            i = i + 1;
        }
        return null;
    }

    static <K, V> Tuple<K, V> choose(HashMap<K, V> m) {
        Map.Entry<K, V> kv = choose(m.entrySet());
        return new Tuple<>(kv.getKey(), kv.getValue());
    }

    static <K, V> List<Tuple<K, V>> choose(HashMap<K, V> m, int n) {
        List<Tuple<K, V>> kv = new ArrayList<>();
        for (int i = 0; i < n; ++i) {
           kv.add(choose(m));
        }
        return kv;
    }

    static <A> List<A> singleton(A x) {
        List<A> xs = new ArrayList<>();
        xs.add(x);
        return xs;
    }

    static <A> List<A> concat(List<A> xs, List<A> ys) {
        List<A> zs = new ArrayList<>();
        zs.addAll(xs);
        zs.addAll(ys);
        return zs;
    }
}
