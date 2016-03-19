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

    public static class Result<O> {

        private Optional<O> ok;
        private Optional<String> error;

        public Result(O okVal) {
            ok = Optional.of(okVal);
            error = Optional.empty();
        }

        public Result(String err) {
            ok = Optional.empty();
            error = Optional.of(err);
        }

        public <X> Result<X> flatmap(Function<O,Result<X>> f) {
            if (this.isOk()) {
                return f.apply(ok.get());
            } else {
                return Result.error(error.get());
            }
        }

        public boolean isOk() {
            return ok.isPresent();
        }

        public static <X> Result<X> ok(X okVal) {
            return new Result<>(okVal);
        }

        public static <X> Result<X> error(String err) {
            return new Result<>(err);
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
}
