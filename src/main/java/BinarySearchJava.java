public class BinarySearchJava {

    public static int binarySearch(int[] nums, int target) {
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

    public static void main(String[] args) {
        int index = binarySearch(new int[] {1,2,3,4,5}, 2);
        System.out.println(index);
    }
}
