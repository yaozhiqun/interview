import java.util.HashMap;
import java.util.Map;

public class SingleNumberJava {

    public static Integer single(int[] nums) {
        Map<Integer, Integer> map = new HashMap();
        for (int i = 0; i < nums.length; i ++) {
            Integer count = map.get(nums[i]);
            if (count == null)
                map.put(nums[i], 1);
            else
                map.put(nums[i], count + 1);
        }

        for (int i = 0; i < nums.length; i ++) {
            if (map.get(nums[i]) == 1)
                return nums[i];
        }
        return null;
    }

    public static int singleNumber(int[] nums) {
        int res = 0;
        for(int num : nums) {
            res ^= num;
        }
        return res;
    }

    public static void main(String[] args) {
        Integer result = single(new int[] { 1, 2, 3, 2, 3, 4, 5, 4, 1, 5});
        System.out.println(result);
        Integer result2 = singleNumber(new int[] { 1, 2, 3, 2, 3, 4, 5, 4, 1});
        System.out.print(result2);
    }
}
