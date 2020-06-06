package old;

public class FirstMissingPositiveJava {
//    public static int firstMissingPositive(int[] nums) {
//        int n = nums.length;
//        int m = n + 1;
//        for (int i = 0; i < n; i++) {
//            if (nums[i] <= 0 || nums[i] > n) nums[i] = 0;
//        }
//        for (int i = 0; i < n; i++) {
//            int prev = nums[i] % m;
//            if (prev > 0)
//                nums[prev - 1] = (prev * m) + nums[prev - 1] % m;
//        }
//        for (int i = 0; i < n; i++) {
//            if (nums[i] / m != i + 1) return i + 1;
//        }
//        return m;
//    }

    public static int firstMissingPositive(int[] nums) {
        int n = nums.length;

        // Base case.
        int contains = 0;
        for (int num : nums)
            if (num == 1) {
                contains++;
                break;
            }

        if (contains == 0)
            return 1;

        // nums = [1]
        if (n == 1)
            return 2;

        // Replace negative numbers, zeros,
        // and numbers larger than n by 1s.
        // After this convertion nums will contain
        // only positive numbers.
        for (int i = 0; i < n; i++)
            if ((nums[i] <= 0) || (nums[i] > n))
                nums[i] = 1;

        // Use index as a hash key and number sign as a presence detector.
        // For example, if nums[1] is negative that means that number `1`
        // is present in the array.
        // If nums[2] is positive - number 2 is missing.
        for (int i = 0; i < n; i++) {
            int a = Math.abs(nums[i]);
            // If you meet number a in the array - change the sign of a-th element.
            // Be careful with duplicates : do it only once.
            if (a == n)
                nums[0] = - Math.abs(nums[0]);
            else
                nums[a] = - Math.abs(nums[a]);
        }

        // Now the index of the first positive number
        // is equal to first missing positive.
        for (int i = 1; i < n; i++) {
            if (nums[i] > 0)
                return i;
        }

        if (nums[0] > 0)
            return n;

        return n + 1;
    }
    public static void main(String[] args) {
        System.out.println(firstMissingPositive(new int[] {1, 2, 3, 4}));
        System.out.println(firstMissingPositive(new int[] {-1, -2, 3, 4}));
        System.out.println(firstMissingPositive(new int[] {2,1,2,-2,5}));
        System.out.println(firstMissingPositive(new int[] {1}));
    }
}
