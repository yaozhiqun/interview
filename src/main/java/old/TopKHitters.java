package old;

import java.util.*;

class HeavyHitter {
   private final String identifier;
   private int frequency;

   HeavyHitter(String identifier, int frequency) {
       this.identifier = identifier;
       this.frequency = frequency;
   }

    public int getFrequency() {
        return frequency;
    }

    @Override
    public String toString() {
        return "HeavyHitter{" +
                "identifier='" + identifier + '\'' +
                ", frequency=" + frequency +
                '}';
    }
}
public class TopKHitters {
    List<HeavyHitter> topK(String[] events, int k) {
        Map<String, Integer> frequencies = new HashMap<>();
        for (String event: events) {
            frequencies.put(event, frequencies.getOrDefault(event, 0) + 1);
        }

        PriorityQueue<HeavyHitter> heap = new PriorityQueue<>(Comparator.comparing(HeavyHitter::getFrequency));

        for (Map.Entry<String, Integer> frequency: frequencies.entrySet()) {
            heap.offer(new HeavyHitter(frequency.getKey(), frequency.getValue()));

            if (heap.size() > k) {
                heap.poll();
            }
        }

        List<HeavyHitter> hitters = new ArrayList<>();
        while (heap.size() > 0) {
            hitters.add(heap.poll());
        }
        return hitters;
    }

    public static void main(String[] args) {
        List<HeavyHitter> hitters = new TopKHitters().topK(new String[] { "a", "b", "a", "c", "d", "e", "e", "e", "f" }, 2);
        hitters.forEach(System.out::println);
    }
}
