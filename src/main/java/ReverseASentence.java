public class ReverseASentence {

    public static String reverse(String sentence) {
        String[] words = sentence.split(" ");
        StringBuilder sb = new StringBuilder();
        for (int i = words.length - 1; i >= 0; i --) {
            sb.append(words[i]);
            if (i > 0)
                sb.append(" ");
        }

        return sb.toString();
    }

    public static void main(String[] args) {
        String reversed = reverse("CCP really sucks");
        System.out.print(reversed);
    }
}
