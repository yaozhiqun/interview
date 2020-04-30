package old;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class QuickSort {

    private static List<Integer> sort(List<Integer> xs) {
        if (xs.size() == 0) {
            return xs;
        }

        int x = xs.get(0);

        List<Integer> smaller = sort(xs.stream().skip(1).filter(i -> i <= x).collect(Collectors.toList()));
        List<Integer> bigger = sort(xs.stream().skip(1).filter(i -> i > x).collect(Collectors.toList()));

        return Stream.concat(
                Stream.concat(smaller.stream(), Stream.of(x)),
                bigger.stream()
        ).collect(Collectors.toList());
    }

    public static void main(String[] args) {
        System.out.println(new ArrayList<>(sort(new ArrayList<>(Arrays.asList(3, 1, 2, 6, 2)))));
    }
}
