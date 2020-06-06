package rocketlawyer;

import java.util.*;
import java.util.stream.Collectors;

public class NthMostRareJ {

    static Optional<Integer> findNthMostRare(List<Integer> xs, Integer nthMostRare) {
        Map<Integer, Integer> appearance = new HashMap<>();
        for (Integer x: xs) {
            appearance.put(x, appearance.getOrDefault(x, 0) + 1);
        }

        if (nthMostRare > appearance.size())
            return Optional.empty();

        Map<Integer, Integer> sorted = appearance.entrySet()
            .stream()
            .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
            .collect(Collectors.toMap(
                        Map.Entry::getKey, Map.Entry::getValue,
                        (oldValue, newValue) -> oldValue,
                        LinkedHashMap::new));

        System.out.println(sorted);

        return Optional.of((Integer) sorted.keySet().toArray()[nthMostRare - 1]);
    }

    public static void main(String[] args) {
        List<Integer> list = Arrays.asList(5, 4, 3, 5, 2, 5, 4, 3, 2, 5, 4, 3, 5, 5, 4, 1);
        System.out.println(findNthMostRare(list, 1));
        System.out.println(findNthMostRare(list, 2));
        System.out.println(findNthMostRare(list, 5));
        System.out.println(findNthMostRare(list, 6));
    }
}

