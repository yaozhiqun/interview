package rocketlawyer;

import static rocketlawyer.ApartmentArranger.ApartmentType.*;

public class ApartmentArranger {

    enum ApartmentType {
        NONE, SMALL, LARGE
    }

    int arrange(int stories) {
        if (stories < 1)
            return 0;

        return rec(1, stories, NONE, 0);
    }

    private int rec(int n, int stories, ApartmentType prevType, int total) {
        if (n > stories) {
            return total + 1;
        } else {
            int nextStory = n + 1;
            switch(prevType) {
                case NONE:
                case LARGE:
                    return rec(nextStory, stories, SMALL, total);
                case SMALL:
                    return rec(nextStory, stories, SMALL, total) + rec(nextStory, stories, LARGE, total);
                default:
                    throw new IllegalStateException("Unknown type of previous apartment type: " + prevType);
            }
        }
    }

    public static void main(String[] args) {
        ApartmentArranger arranger = new ApartmentArranger();
        System.out.println(arranger.arrange(1));
        System.out.println(arranger.arrange(2));
        System.out.println(arranger.arrange(3));
        System.out.println(arranger.arrange(4));
        System.out.println(arranger.arrange(20));
    }
}
