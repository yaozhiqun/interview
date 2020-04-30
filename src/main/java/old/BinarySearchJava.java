package old;

public class BinarySearchJava {

    private static int binarySearch(int[] nums, int target) {
        int left = 0;
        int right = nums.length - 1;
        while (left <= right) {
            int mid = left + (right - left) / 2;
            if (target < nums[mid]) {
                right = mid - 1;
            } else if (target > nums[mid])
                left = mid + 1;
            else
                return mid;
        }
        return  -1;
    }

    private static int binRecurSearch(int[] xs, int x) {
        return recurSearch(xs, x, 0, xs.length - 1);
    }

    private static int recurSearch(int[] xs, int x, int low, int high) {
        if (low > high) {
            return high;
        } else {
            int middle = (low + high) / 2;
            if (xs[middle] == x) {
                return middle;
            } else if (xs[middle] > x) {
                return recurSearch(xs, x, low, middle - 1);
            } else {
                return recurSearch(xs, x, middle + 1, high);
            }
        }
    }

    public static void main(String[] args) {
//        int index = binarySearch(new int[] {1,2,3,4,5,7}, 6);
        int index = binRecurSearch(new int[] {1,2,3,4,5,7}, 2);
        System.out.println(index);
    }
}
