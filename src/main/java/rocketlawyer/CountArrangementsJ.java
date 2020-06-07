package rocketlawyer;

public class CountArrangementsJ {

    static int countArrangements(int stories) {
        if (stories < 1)
            return 0;

        return rec(1, stories, -1, 0);
    }

    private static int rec(int n, int stories, int last, int total) {
        if (n > stories) {
            return total + 1;
        } else {
            int nextStory = n + 1;
            if (last == -1) {
                return rec(nextStory, stories, 0, total);
            } else {
                if (last == 0) {
                    return rec(nextStory, stories, 0, total) + rec(nextStory, stories, 1, total);
                } else {
                    return rec(nextStory, stories, 0, total);
                }
            }
        }
    }

    public static void main(String[] args) {
        System.out.println(countArrangements(1));
        System.out.println(countArrangements(2));
        System.out.println(countArrangements(3));
        System.out.println(countArrangements(4));
        System.out.println(countArrangements(20));
    }
}
