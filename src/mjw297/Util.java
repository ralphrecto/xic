package mjw297;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by ralphrecto on 2/14/16.
 */
public class Util {

    public static class Tuple<S,T> {
        public final S fst;
        public final T snd;

        public Tuple(S fst, T snd) {
            this.fst = fst;
            this.snd = snd;
        }
    }

    static <X,Y> List<Tuple<X,Y>> zip(List<X> fsts, List<Y> snds) {
        List<Tuple<X,Y>> ret = new ArrayList<>();
        for (int i = 0; i < Math.min(fsts.size(), snds.size()); i++) {
            ret.add(new Tuple<>(fsts.get(i), snds.get(i)));
        }
        return ret;
    }

}
