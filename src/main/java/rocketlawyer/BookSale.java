package rocketlawyer;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class BookSale {

    public static int nthLowestSelling(int[] sales, int n) {
        Map<Integer, Integer> occurrence = new HashMap<>();
        for (int sale: sales) {
            occurrence.put(sale, occurrence.getOrDefault(sale, 0) + 1);
        }

        if (n > occurrence.size())
            return -1;

        Map<Integer, Integer> sorted = occurrence.entrySet()
                .stream()
                .sorted(Map.Entry.comparingByValue())
                .collect(Collectors.toMap(
                        Map.Entry::getKey, Map.Entry::getValue,
                        (oldValue, newValue) -> oldValue,
                        LinkedHashMap::new));


        return (Integer) sorted.keySet().toArray()[n - 1];
    }

    public static void main(String[] args) {
        int x = nthLowestSelling(new int[] { 5, 4, 3, 2, 1, 5, 4, 3, 2, 5, 4, 3, 5, 4, 5 }, 2);
        System.out.println(x);
    }
}


