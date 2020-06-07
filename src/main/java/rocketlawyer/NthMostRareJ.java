package rocketlawyer;

import java.util.*;
import java.util.stream.Collectors;

public class NthMostRareJ {

    Optional<Integer> findNthMostRare(List<Integer> xs, Integer nthMostRare) {
        Map<Integer, Integer> occurrence = new HashMap<>();
        for (Integer x: xs) {
            occurrence.put(x, occurrence.getOrDefault(x, 0) + 1);
        }

        if (nthMostRare > occurrence.size())
            return Optional.empty();

        Map<Integer, Integer> sorted = occurrence.entrySet()
            .stream()
            .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
            .collect(Collectors.toMap(
                        Map.Entry::getKey, Map.Entry::getValue,
                        (oldValue, newValue) -> oldValue,
                        LinkedHashMap::new));

//        System.out.println(sorted);

        return Optional.of((Integer) sorted.keySet().toArray()[nthMostRare - 1]);
    }

    public static void main(String[] args) {
        List<Integer> list = Arrays.asList(5, 4, 3, 5, 2, 5, 4, 3, 2, 5, 4, 3, 5, 5, 4, 1);
        NthMostRareJ finder = new NthMostRareJ();
        System.out.println(finder.findNthMostRare(list, 1));
        System.out.println(finder.findNthMostRare(list, 2));
        System.out.println(finder.findNthMostRare(list, 5));
        System.out.println(finder.findNthMostRare(list, 6));
    }
}

