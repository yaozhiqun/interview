package old;

import java.util.HashMap;
import java.util.Map;

public class SumTwo {

    public static int[] sumTwo(int[] nums, int target) {
        int[] result = new int[2];
        Map<Integer, Integer> map = new HashMap();
        for (int i = 0; i < nums.length; i ++) {
            int x = target - nums[i];
            if (map.containsKey(x)) {
                result[0] = i;
                result[1] = map.get(x);
                break;
            } else {
                map.put(nums[i], i);
            }
        }
        return result;
    }

    public static void main(String[] args) {
        int[] result = sumTwo(new int[] { 2, 7, 11, 15 }, 18);
        System.out.println(result[0] + " " + result[1]);
    }
}
