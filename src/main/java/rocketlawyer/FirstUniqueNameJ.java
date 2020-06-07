package rocketlawyer;

import java.util.*;

public class FirstUniqueNameJ {

    class Stat {
        int count;
        int index;

        Stat(int count, int index) {
            this.count = count;
            this.index = index;
        }

        Stat setIndex(int index) {
            this.index = index;
            return this;
        }

        Stat setCount(int count) {
            this.count = count;
            return this;
        }

        int getIndex() {
            return this.index;
        }
    }

    String find(List<String> names) {
        Map<String, Stat> occurrence = new HashMap<>();
        for (int i = 0; i < names.size(); i++) {
            Stat stat = occurrence.getOrDefault(names.get(i), new Stat(0, i));
            Stat updatedStat = stat.setCount(stat.count + 1).setIndex(i);
            occurrence.put(names.get(i), updatedStat);
        }

        return occurrence.entrySet()
                .stream()
                .filter(entry -> entry.getValue().count == 1)
                .sorted(Map.Entry.comparingByValue(Comparator.comparing(Stat::getIndex)))
                .findFirst()
                .map(Map.Entry::getKey)
                .orElse(null);
    }

    public static void main(String[] args) {
        FirstUniqueNameJ finder = new FirstUniqueNameJ();
        System.out.println(finder.find(Arrays.asList( "Abbi", "Adeline", "Abbi", "Adalia")));
        System.out.println(finder.find(Arrays.asList( "Abbi", "Adeline", "Abbi", "Adalia", "Adeline")));
        System.out.println(finder.find(Arrays.asList( "Abbi", "Adeline", "Abbi", "Adalia", "Adeline", "Adalia")));
    }
}
