package mjw297;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
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
