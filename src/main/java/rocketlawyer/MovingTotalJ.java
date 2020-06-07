package rocketlawyer;

import java.util.Arrays;
import java.util.List;

class MovingTotalJ {
    boolean exists(List<Integer> xs, int target) {
        if (xs.size() >= 3) {
            for (int i = 2; i < xs.size(); i++) {
                if ((xs.get(i) + xs.get(i - 1) + xs.get(i - 2)) == target)
                    return true;
            }
        }
        return false;
    }
}

class MovingTotalJMain {
    public static void main(String[] args) {
        System.out.println(new MovingTotalJ().exists(Arrays.asList(1, 2, 3), 6));
        System.out.println(new MovingTotalJ().exists(Arrays.asList(1, 2, 3), 9));
        System.out.println(new MovingTotalJ().exists(Arrays.asList(1, 2, 3, 4), 9));
        System.out.println(new MovingTotalJ().exists(Arrays.asList(1, 2, 3, 4), 7));
    }
}

