package old;

public class ReverseArray {

    public static void main(String[] args) {
        String str = "Hello Java";
        char[] chars = str.toCharArray();
        char[] reversed = new char[chars.length];
        for (int left = 0, right = chars.length - 1; left < chars.length; left++, right--) {
            reversed[left] = chars[right];
        }
        System.out.print(reversed);
    }
}
