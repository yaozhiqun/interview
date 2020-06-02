package old;

// number of prefix reversals to
// sort permutation of first N numbers
import java.util.*;

public class Main {

    // function to find minimum prefix reversal through BFS
    public static int minimumPrefixReversals(int[] a) {
        // size of array
        int n = a.length;

        // string for initial and goal nodes
        String start = "", destination = "";

        // string for manipulation in while loop
        String original = "", modified = "";

        // node to store temporary values from front of queue
        Node temp = null;

        // create the starting string
        for (int i = 0; i < n; i++)
            start += a[i];

        // sort the array and prepare final destination string
        Arrays.sort(a);
        for (int i = 0; i < n; i++)
            destination += a[i];

        // this queue will store all the BFS siblings
        Queue<Node> q = new LinkedList<>();

        // place the starting node in queue
        q.add(new Node(start, 0));

        //base case:- if array is already sorted
        if (start == destination)
            return 0;


        // loop until the size of queue is empty
        while (q.size() != 0) {
            // put front node of queue in temporary variable
            temp = q.poll();

            // store the original string at this step
            original = temp.string;

            for (int j = 2; j <= n; j++) {
                // modified will be used to genrate all
                // manipulation of original string
                // like if original = 1342
                // modified = 3142 , 4312 , 2431

                modified = original;

                // generate the permutation by reversing
                modified = reverse(modified, j);

                if (modified.equals(destination)) {
                    // if string match then return
                    // the height of the current node
                    return temp.steps + 1;
                }

                // else put this node into queue
                q.add(new Node(modified, temp.steps + 1));
            }

        }

        // if no case match then default value
        return Integer.MIN_VALUE;
    }

    // function to reverse the string upto an index
    public static String reverse(String s, int index) {
        char temp[] = s.toCharArray();
        int i = 0;
        while (i < index) {
            char c = temp[i];
            temp[i] = temp[index - 1];
            temp[index - 1] = c;
            i++;
            index--;
        }
        return String.valueOf(temp);
    }

    // Driver code
    public static void main(String[] args) {
//        int a[] = new int[]{1, 2, 4, 3};
        int a[] = new int[]{3, 1, 2};
        System.out.println(minimumPrefixReversals(a));
    }

    // Node class to store a combined set of values
    static class Node {
        public String string;
        public int steps;

        public Node(String string, int steps) {
            this.string = string;
            this.steps = steps;
        }
    }
}
